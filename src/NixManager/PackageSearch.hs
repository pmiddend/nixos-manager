{-# LANGUAGE OverloadedStrings #-}
module NixManager.PackageSearch
  ( searchPackages
  )
where

import           Data.ByteString.Lazy           ( hGetContents )
import           NixManager.Util                ( addToError
                                                , MaybeError
                                                )
import           System.Process                 ( createProcess
                                                , proc
                                                , std_out
                                                , StdStream(CreatePipe)
                                                )
import           Data.Text                      ( Text )
import           NixManager.NixPackage          ( NixPackage
                                                , readPackages
                                                )
import           Control.Lens                   ( (^.) )
import           Data.Text.Lens                 ( unpacked )

searchPackages :: Text -> IO (MaybeError [NixPackage])
searchPackages t = do
  (_, Just hout, _, _) <- createProcess
    (proc "nix" ["search", t ^. unpacked, "--json"]) { std_out = CreatePipe }
  out <- hGetContents hout
  pure
    (addToError
      "Error parsing output of \"nix search\" command. This could be due to changes in this command in a later version (and doesn't fix itself). Please open an issue in the nixos-manager repository. The error was: "
      (readPackages out)
    )

