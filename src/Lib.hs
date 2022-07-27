-- | Owl as a library.
--   This module simply exposes the Owl Api, and thus, only consists of import and export statements.
module Lib
    ( -- * OwlCore #OwlCore#
      -- | The OwlCore Repl.
      module OwlCore.Repl.Repl
    ) where


-- | The repl.
import OwlCore.Repl.Repl
