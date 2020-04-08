{-|
  Description: Tools to parse and transform the Docbook descriptions for, e.g. services, into GTK pango markup (see https://developer.gnome.org/pango/stable/pango-Markup.html)
  #-}
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
import           NixManager.Util                ( TextualError
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

-- | Parse a docbook string into a valid 'Document' (or return an error)
parseDocbook :: Text -> TextualError Document
parseDocbook =
  addToError "error parsing documentation: "
    . fromShowableError
    . parseText def
    . fromStrict
    . surroundSimple "root" -- the XML parser needs a root element

-- | Stupidly replace HTML entities by their ampersandish equivalents (GTK will complain otherwise). This function possibly misses entities, I haven't look them all up.
replaceEntities :: Endo Text
replaceEntities =
  replace "<" "&lt;" . replace ">" "&gt;" . replace "\"" "&quot;"

-- | Convert Docbook XML to Pango XML
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
