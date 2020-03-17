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
  , ".error-message { background-image: image(#e1d08e); }"
  , ".info-message { background-image: image(#bef7ff); }"
  , ".service-headline { font-size: 40px; }"
  , ".service-option-title { font-family: monospace; font-weight: bold; }"
  , ".service-option-description { font-style: italic; }"
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
