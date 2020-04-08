{-|
  Description: Provides an enum for the mode of operation for rebuild in the GUI
  -}
module NixManager.NixRebuildUpdateMode
  ( NixRebuildUpdateMode(..)
  )
where

-- | How the GUI shall call @nixos-rebuild@
data NixRebuildUpdateMode = NixRebuildUpdateNone -- ^ Call it with neither @--rollback@ nor @--upgrade@
                          | NixRebuildUpdateUpdate -- ^ Call it with @--upgrade@
                          | NixRebuildUpdateRollback-- ^ Call it with @--rollback@
                          deriving(Eq, Enum, Bounded)
