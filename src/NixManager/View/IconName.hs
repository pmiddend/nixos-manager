module NixManager.View.IconName
  ( IconName(..)
  )
where

data IconName = SystemRun
              | SystemSoftwareInstall
              | EditDelete
              | ProcessStop
              | ViewRefresh
              deriving(Eq)

instance Show IconName where
  show SystemRun             = "system-run"
  show SystemSoftwareInstall = "system-software-install"
  show EditDelete            = "edit-delete"
  show ProcessStop           = "process-stop"
  show ViewRefresh           = "view-refresh"
