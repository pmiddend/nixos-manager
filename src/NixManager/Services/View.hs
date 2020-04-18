{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
  Description: Contains the actual GUI (widgets) for the services tab
Contains the actual GUI (widgets) for the services tab
  -}
module NixManager.Services.View
  ( servicesBox
  , noticeBox
  )
where

import           NixManager.View.ServiceEditView
                                                ( editView )
import           NixManager.View.InformationBox ( informationBox )
import           NixManager.View.ImageButton    ( imageButton )
import qualified NixManager.View.IconName      as IconName
import           Data.Default                   ( def )
import           NixManager.View.GtkUtil        ( expandAndFill )
import           NixManager.View.ProgressBar    ( progressBar )
import           Data.Text                      ( Text )
import           GI.Gtk.Declarative             ( bin
                                                , on
                                                , BoxChild(BoxChild)
                                                , defaultBoxChildProperties
                                                , widget
                                                , Attribute((:=))
                                                , container
                                                )
import qualified GI.Gtk                        as Gtk
import           Control.Lens                   ( (^.) )
import           NixManager.Services.Event      ( Event
                                                  ( EventEditView
                                                  , EventDownloadStart
                                                  , EventStateReload
                                                  , EventDownloadCancel
                                                  )
                                                )
import           NixManager.Services.State      ( State
                                                  ( StateInvalidOptions
                                                  , StateInvalidExpr
                                                  , StateDone
                                                  , StateDownloading
                                                  )
                                                , sddCounter
                                                )
import           NixManager.ManagerEvent        ( ManagerEvent
                                                  ( ManagerEventServices
                                                  )
                                                )
import           NixManager.ManagerState        ( msServiceState )

--servicesBox :: ManagerState -> Widget ManagerEvent
-- This extra container is there to circumvent a bug that switches to the next page when one page is replaced.
servicesBox s = container
  Gtk.Box
  []
  [BoxChild expandAndFill (servicesBox' (s ^. msServiceState) s)]

-- | What to display when the service definitions couldn't be parsed
invalidOptionsMessage :: Maybe Text -> Text
invalidOptionsMessage (Just e) =
  "Service definition file is invalid, possibly because of a corrupt download. You should try again. The error is:\n\n"
    <> e
invalidOptionsMessage Nothing =
  "Service definitions need to be downloaded first.\nPress the button below to start the download. It'll only take a few seconds, depending on your internet speed."

-- | The icon to display in case the service definitions aren't there or invalid
invalidOptionsIcon (Just _) = IconName.DialogError
invalidOptionsIcon Nothing  = IconName.EmblemDocuments

-- | The button text to display in case the service definitions aren't there or invalid
invalidOptionsButtonText (Just _) = "Retry Download"
invalidOptionsButtonText Nothing  = "Start Download"

-- | General function to display the notice box in case the service definitions aren't there or are invalid
noticeBox icon buttonEvent buttonIcon buttonText message = container
  Gtk.Box
  [ #orientation := Gtk.OrientationVertical
  , #spacing := 10
  , #marginLeft := 40
  , #marginRight := 40
  , #marginTop := 5
  ]
  [ BoxChild def (informationBox False icon message)
  , BoxChild
    def
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignCenter]
      [ BoxChild def $ imageButton
          [ #label := buttonText
          , on #clicked buttonEvent
          , #alwaysShowImage := True
          ]
          buttonIcon
      ]
    )
  ]

-- | The services tab root
servicesBox' (StateDownloading ssdd) _ = container
  Gtk.Box
  [ #orientation := Gtk.OrientationVertical
  , #spacing := 10
  , #marginLeft := 40
  , #marginRight := 40
  , #marginTop := 5
  ]
  [ BoxChild defaultBoxChildProperties
             (widget Gtk.Label [#label := "Downloading services..."])
  , BoxChild defaultBoxChildProperties (progressBar [] (ssdd ^. sddCounter))
  , BoxChild
    defaultBoxChildProperties
    (container
      Gtk.Box
      [#orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignCenter]
      [ widget
          Gtk.Button
          [ #label := "Cancel"
          , on #clicked (ManagerEventServices EventDownloadCancel)
          ]
      ]
    )
  ]
servicesBox' (StateInvalidExpr e) _ = bin Gtk.ScrolledWindow [] $ noticeBox
  IconName.DialogError
  (ManagerEventServices EventStateReload)
  IconName.EmblemDownloads
  "Reload service state"
  ("Your service expression file is not valid. Maybe you have edited it by hand and it's become corrupted?\nPlease fix the error and then press the button below. The error is:\n"
  <> e
  )
servicesBox' (StateInvalidOptions possibleError) _ =
  bin Gtk.ScrolledWindow [] $ noticeBox
    (invalidOptionsIcon possibleError)
    (ManagerEventServices EventDownloadStart)
    IconName.EmblemDownloads
    (invalidOptionsButtonText possibleError)
    (invalidOptionsMessage possibleError)
servicesBox' (StateDone sd) s =
  ManagerEventServices . EventEditView <$> editView sd s
