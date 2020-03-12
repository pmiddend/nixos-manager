{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

import           NixManager.Css
import           Data.Fix                       ( Fix(Fix)
                                                , cata
                                                )
import           Data.Text.IO                   ( putStrLn )
import qualified Data.Text                     as Text
import           Control.Lens                   ( (^.)
                                                , ix
                                                , to
                                                , (^..)
                                                , (&)
                                                , (?~)
                                                , (^?!)
                                                , (.~)
                                                , folded
                                                , filtered
                                                )
import           NixManager.ManagerState
import           NixManager.Nix
import           NixManager.Util
import           NixManager.NixParser
import           NixManager.ManagerEvent
import           NixManager.View
import           GI.Gtk.Declarative.App.Simple  ( App(App)
                                                , view
                                                , update
                                                , Transition(Transition, Exit)
                                                , inputs
                                                , initialState
                                                , run
                                                )
import           Control.Monad                  ( void )
import qualified GI.Gtk                        as Gtk
import           Data.Text                      ( toLower
                                                , isInfixOf
                                                , Text
                                                )
import           Prelude                 hiding ( length
                                                , putStrLn
                                                )

packageMatches :: Text -> NixPackage -> Bool
packageMatches t p = toLower t `isInfixOf` (p ^. npName . to toLower)

pureTransition :: ManagerState -> Transition ManagerState ManagerEvent
pureTransition x = Transition x (pure Nothing)

update' :: ManagerState -> ManagerEvent -> Transition ManagerState ManagerEvent
update' _ ManagerEventClosed = Exit
update' s (ManagerEventPackageSelected (Just i)) =
  pureTransition (s & msSelectedPackage ?~ (s ^?! msPackageSearchResult . ix i))
update' s (ManagerEventPackageSelected _) =
  pureTransition (s & msSelectedPackage .~ Nothing)
update' s (ManagerEventSearchChanged t) = pureTransition
  (  s
  &  msSearchString
  .~ t
  &  msPackageSearchResult
  .~ (s ^.. msPackageCache . folded . filtered (packageMatches t))
  )

readInstalledPackages :: IO [Text]
readInstalledPackages = do
  expr <- parseFile "packages.nix"
  case expr of
    Right (Fix (NixFunctionDecl _ (Fix (NixSet m)))) ->
      pure
        (   Text.drop 5
        <$> cata evalSymbols (m ^?! ix "environment.systemPackages")
        )
    Left e -> do
      putStrLn ("parse error " <> e)
      error "parse error"
    _ -> error "invalid packages.nix"

readCache :: IO [NixPackage]
readCache = do
  cache             <- nixSearchUnsafe ""
  installedPackages <- readInstalledPackages
  pure
    $   (\ip -> ip & npInstalled .~ ((ip ^. npName) `elem` installedPackages))
    <$> cache

nixMain :: IO ()
nixMain = do
  void (Gtk.init Nothing)
  putStrLn "Reading cache..."
  putStrLn "Starting..."
  initCss
  cache <- readCache
  void $ run App
    { view         = view'
    , update       = update'
    , inputs       = []
    , initialState = ManagerState { _msPackageCache        = cache
                                  , _msSearchString        = mempty
                                  , _msPackageSearchResult = mempty
                                  , _msSelectedPackage     = Nothing
                                  }
    }
