module NixManager.Util where

import           Data.Text                      ( Text
                                                , pack
                                                )

showText :: Show a => a -> Text
showText = pack . show

mwhen :: Monoid m => Bool -> m -> m
mwhen True  v = v
mwhen False _ = mempty

type Endo a = a -> a

