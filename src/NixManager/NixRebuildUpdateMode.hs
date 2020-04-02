module NixManager.NixRebuildUpdateMode
  ( NixRebuildUpdateMode(..)
  )
where

data NixRebuildUpdateMode = NixRebuildUpdateNone
                          | NixRebuildUpdateUpdate
                          | NixRebuildUpdateRollback
                          deriving(Eq, Enum, Bounded)
