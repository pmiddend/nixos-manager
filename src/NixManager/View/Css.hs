{-# LANGUAGE OverloadedStrings #-}
module NixManager.View.Css where

import           Data.ByteString                ( ByteString )
import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk


styles :: ByteString
styles = mconcat
  [ ".package-row-installed { background-color: #cdffcd; }"
  , ".install-button { background-image: image(#cdffcd); font-weight: bold; }"
  , ".try-install-button { background-image: image(#cdffcd); }"
  , ".remove-button { background-image: image(#ffaeae); }"
  , ".error-message { background-image: image(#fff3cd); }"
  , ".info-message { background-image: image(#bef7ff); }"
  , ".package-row-even { }"
  , ".package-row-odd { background-color: #f2f2f2; }"
  , ".service-headline { font-size: 40px; }"
  , ".service-option-title { font-family: monospace; font-weight: bold; }"
  , ".unspecified-label { color: #101010; }"
  , ".startup-error-message { font-family: monospace; }"
  , ".option-type-description { font-style: italic; font-size: 13px; }"
  , ".nixos-manager-headline { font-size: 30px; }"
  , ".nixos-manager-italic { font-style: italic; }"
  , ".nixos-manager-monospace { font-family: monospace; }"
  , ".nixos-manager-grey-background { background-color: #eeeeee; }"
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
