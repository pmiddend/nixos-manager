{-# LANGUAGE OverloadedStrings #-}
module NixManager.Css where

import           Data.ByteString                ( ByteString )
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk


styles :: ByteString
styles = mconcat
  [ ".package-row-installed { background-color: #cdffcd; }"
  , ".install-button { background-image: image(#cdffcd); font-weight: bold; }"
  , ".try-install-button { background-image: image(#cdffcd); }"
  , ".remove-button { background-image: image(#ffaeae); }"
  ]

initCss :: IO ()
initCss = do
  cssProvider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromData cssProvider styles
  screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
  Gtk.styleContextAddProviderForScreen
    screen
    cssProvider
    (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
