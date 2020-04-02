module NixManager.Admin.DetailsState
  ( DetailsState(..)
  , detailsBool
  )
where

import           Control.Lens                   ( Iso'
                                                , iso
                                                )

data DetailsState = DetailsContracted
                  | DetailsExpanded
                  deriving(Eq, Bounded, Enum)

detailsBool :: Iso' DetailsState Bool
detailsBool = iso toBool fromBool
 where
  toBool DetailsContracted = False
  toBool DetailsExpanded   = True
  fromBool False = DetailsContracted
  fromBool True  = DetailsExpanded

