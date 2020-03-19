{-# LANGUAGE OverloadedStrings #-}
module NixManager.PackageSearch
  ( searchPackages
  )
where

import           Data.ByteString.Lazy           ( ByteString
                                                , hGetContents
                                                )
import           NixManager.Util                ( fromEither
                                                , addToError
                                                , MaybeError
                                                )
import           System.Process                 ( createProcess
                                                , proc
                                                , std_out
                                                , StdStream(CreatePipe)
                                                )
import           Data.Map.Strict                ( Map
                                                , elems
                                                )
import           Data.Text                      ( Text )
import           NixManager.NixPackage          ( NixPackage )
import           Data.Aeson                     ( eitherDecode )
import           Control.Lens                   ( (^.) )
import           Data.Text.Lens                 ( unpacked )

decodeNixSearchResult :: ByteString -> MaybeError (Map Text NixPackage)
decodeNixSearchResult = fromEither . eitherDecode

searchPackages :: Text -> IO (MaybeError [NixPackage])
searchPackages t = do
  (_, Just hout, _, _) <- createProcess
    (proc "nix" ["search", t ^. unpacked, "--json"]) { std_out = CreatePipe }
  out <- hGetContents hout
  pure
    (addToError
      "Error parsing output of \"nix search\" command. This could be due to changes in this command in a later version (and doesn't fix itself). Please open an issue in the nixos-manager repository. The error was: "
      (elems <$> decodeNixSearchResult out)
    )

