{-|
  Description: Contains "DetailsState" to signify if a GTK expander is contracted
Contains "DetailsState" to signify if a GTK expander is contracted
  -}
{-# LANGUAGE DeriveGeneric #-}
module NixManager.View.DetailsState
  ( DetailsState(..)
  , detailsBool
  )
where

import           Control.Lens                   ( Iso'
                                                , iso
                                                )
import           GHC.Generics                   ( Generic )

-- | Signifies if an expander is contracted or expanded
data DetailsState = DetailsContracted
                  | DetailsExpanded
                  deriving(Eq, Bounded, Enum, Generic)

-- | Isomorphism to boolean (contracted being @false@)
detailsBool :: Iso' DetailsState Bool
detailsBool = iso toBool fromBool
 where
  toBool DetailsContracted = False
  toBool DetailsExpanded   = True
  fromBool False = DetailsContracted
  fromBool True  = DetailsExpanded

