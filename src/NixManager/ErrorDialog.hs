{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module NixManager.ErrorDialog
  ( runErrorDialog
  )
where

import           Text.Wrap                      ( wrapText
                                                , WrapSettings(WrapSettings)
                                                )
import           Data.Text                      ( Text )
import           Control.Monad                  ( void )
import           GI.Gtk.Declarative.App.Simple  ( App(App)
                                                , view
                                                , update
                                                , AppView
                                                , Transition(Exit)
                                                , inputs
                                                , initialState
                                                , run
                                                )
import           GI.Gtk.Declarative             ( bin
                                                , padding
                                                , defaultBoxChildProperties
                                                , on
                                                , expand
                                                , container
                                                , fill
                                                , widget
                                                , Attribute((:=))
                                                , classes
                                                , container
                                                , BoxChild(BoxChild)
                                                , on
                                                )
import qualified GI.Gtk                        as Gtk

data Event = ExitEvent

errorDialog :: Text -> () -> AppView Gtk.Dialog Event
errorDialog e _ =
  let msgLabel = widget
        Gtk.Label
        [ #label := wrapText (WrapSettings True False) 80 e
        , classes ["startup-error-message"]
        ]
      paddedAround spacing =
          container Gtk.Box [#orientation := Gtk.OrientationVertical]
            . pure
            . BoxChild defaultBoxChildProperties { padding = spacing
                                                 , expand  = True
                                                 , fill    = True
                                                 }
            . container Gtk.Box []
            . pure
            . BoxChild defaultBoxChildProperties { padding = spacing
                                                 , expand  = True
                                                 , fill    = True
                                                 }
  in  bin
          Gtk.Dialog
          [ #title := "An error occurred"
          , on #deleteEvent (const (True, ExitEvent))
          , #widthRequest := 300
          , #heightRequest := 200
          ]
        $ container
            Gtk.Box
            [#orientation := Gtk.OrientationVertical]
            [ BoxChild
              defaultBoxChildProperties { expand = True, fill = True }
              (paddedAround 20 msgLabel)
            , paddedAround 5 $ container
              Gtk.Box
              [#halign := Gtk.AlignEnd, #spacing := 5]
              [ widget
                  Gtk.Button
                  [ #label := "Okay, let me fix this real quick"
                  , on #clicked ExitEvent
                  ]
              ]
            ]

runErrorDialog :: Text -> IO ()
runErrorDialog e = void $ run App { view         = errorDialog e
                                  , update       = \_ _ -> Exit
                                  , inputs       = []
                                  , initialState = ()
                                  }
