{-# LANGUAGE OverloadedStrings #-}
module NixManager.Docbook
  ( parseDocbook
  , docbookToPango
  )
where

import           Data.Text                      ( Text
                                                , replace
                                                )
import           Text.XML                       ( parseText
                                                , psDecodeEntities
                                                , Element
                                                , Document
                                                , Node
                                                  ( NodeContent
                                                  , NodeComment
                                                  , NodeElement
                                                  , NodeInstruction
                                                  )
                                                )
import           Data.Default                   ( def )
import           NixManager.Util                ( MaybeError
                                                , Endo
                                                , surroundSimple
                                                , fromShowableError
                                                , addToError
                                                )
import           Control.Lens                   ( view
                                                , to
                                                , (^.)
                                                , plate
                                                , folded
                                                )
import           Text.XML.Lens                  ( nodes
                                                , localName
                                                , root
                                                , text
                                                , attr
                                                , named
                                                )
import           Data.Text.Lazy                 ( fromStrict )

parseDocbook :: Text -> MaybeError Document
parseDocbook =
  addToError "error parsing documentation: "
    . fromShowableError
    . parseText def
    . fromStrict
    . surroundSimple "root"

replaceEntities :: Endo Text
replaceEntities =
  replace "<" "&lt;" . replace ">" "&gt;" . replace "\"" "&quot;"

docbookToPango :: Document -> Text
docbookToPango rootNode = rootNode ^. root . nodes . folded . to nodeToPango
 where
  nodeToPango :: Node -> Text
  nodeToPango (NodeElement     e) = nodeElementToPango (e ^. localName) e
  nodeToPango (NodeContent     t) = replaceEntities t
  nodeToPango (NodeInstruction _) = ""
  nodeToPango (NodeComment     _) = ""
  makeTt = surroundSimple "tt" . replaceEntities . view text
  nodeElementToPango :: Text -> Element -> Text
  nodeElementToPango "link"           e = e ^. attr "href"
  nodeElementToPango "filename"       e = makeTt e
  nodeElementToPango "literal"        e = makeTt e
  nodeElementToPango "command"        e = makeTt e
  nodeElementToPango "option"         e = makeTt e
  nodeElementToPango "code"           e = makeTt e
  nodeElementToPango "programlisting" e = makeTt e
  nodeElementToPango "varname"        e = makeTt e
  nodeElementToPango "citerefentry" e =
    "man "
      <> (e ^. plate . named "manvolnum" . text)
      <> " "
      <> (e ^. plate . named "refentrytitle" . text)
  nodeElementToPango _ e = e ^. nodes . folded . to nodeToPango
