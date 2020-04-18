{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module NixManager.PseudoList(PseudoList, elems, fromList, append, filter) where

import qualified Data.Map.Strict as Map
import NixManager.Util(Endo, safeMaximum)
import Data.Maybe(fromMaybe)
import Prelude hiding (filter)
import Control.Lens(Index, IxValue, makeLenses, At, at, Iso', iso, from, mapping, au, auf, under, Ixed, ix, view, Lens')
import qualified Prelude

newtype PseudoList a = PseudoList (Map.Map Int a)
                     deriving(Show, Eq, Foldable, Functor, Traversable, Semigroup, Monoid)

_PseudoList :: Iso' (PseudoList a) (Map.Map Int a)
_PseudoList = iso (\(PseudoList a) -> a) PseudoList 

elems :: PseudoList a -> [a]
elems (PseudoList a) = Map.elems a

fromList :: [a] -> PseudoList a
fromList = PseudoList . Map.fromList . zip [0..]

append :: a -> Endo (PseudoList a)
append a (PseudoList xs) = PseudoList (Map.insert (fromMaybe 0 (safeMaximum (Map.keys xs))) a xs)

filter :: (a -> Bool) -> Endo (PseudoList a)
filter f = fromList . Prelude.filter f . elems

type instance Index (PseudoList a) = Int
type instance IxValue (PseudoList a) = a

instance Ixed (PseudoList a) where
  ix i = _PseudoList . ix i

instance At (PseudoList a) where
  at i = _PseudoList . at i
