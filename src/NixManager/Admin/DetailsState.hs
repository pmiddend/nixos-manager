{-|
  Description: Contains "DetailsState" to signify if a GTK expander is contracted
  -}
module NixManager.Admin.DetailsState
  ( DetailsState(..)
  , detailsBool
  )
where

import           Control.Lens                   ( Iso'
                                                , iso
                                                )

-- | Signifies if an expander is contracted or expanded
data DetailsState = DetailsContracted
                  | DetailsExpanded
                  deriving(Eq, Bounded, Enum)

-- | Isomorphism to boolean (contracted being @false@)
detailsBool :: Iso' DetailsState Bool
detailsBool = iso toBool fromBool
 where
  toBool DetailsContracted = False
  toBool DetailsExpanded   = True
  fromBool False = DetailsContracted
  fromBool True  = DetailsExpanded

