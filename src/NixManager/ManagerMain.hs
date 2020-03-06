{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ManagerMain where

import           Control.Monad                  ( forM_ )
import qualified GI.Gtk                        as Gtk
import           Data.GI.Base                   ( new
                                                , on
                                                , AttrOp((:=))
                                                )
import           System.Environment             ( getArgs )
import           Data.Text                      ( pack )

createRow t = do
  rowBox      <- Gtk.boxNew Gtk.OrientationHorizontal 0
  rowBoxLabel <- Gtk.labelNew (Just t)
  #add rowBox rowBoxLabel
  resultingRow <- Gtk.listBoxRowNew
  #add resultingRow rowBox
  pure resultingRow

nixMain :: IO ()
nixMain = do
  putStrLn "Starting..."
  argv             <- getArgs
  (initSuccess, _) <- Gtk.initCheck (Just (pack <$> argv))
  if initSuccess
    then do
      win          <- new Gtk.Window [#title := "nix-manager"]
      _            <- on win #destroy Gtk.mainQuit
      mainNotebook <- new Gtk.Notebook []
      #add win mainNotebook
      packagesScrolled <- new Gtk.ScrolledWindow []
      runLabel         <- new Gtk.Label [#label := "Packages"]
      iRun             <- Gtk.notebookAppendPage mainNotebook
                                                 packagesScrolled
                                                 (Just runLabel)
      packageList <- Gtk.listBoxNew
      #add packagesScrolled packageList
      forM_ [0 .. 10000] $ \i -> do
        r <- createRow ("row" <> pack (show i))
        #add packageList r
      #showAll win
      Gtk.main
    else putStrLn "Init failed"
