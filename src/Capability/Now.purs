module Capability.Now where

import Prelude

import Data.DateTime (Date, DateTime, Time)
import Data.DateTime.Instant (Instant)
import Halogen (HalogenM, lift)

--- /////////////////////////////////////////////////////////////////////// ---

class Monad m <= Now m where
  now         :: m Instant
  nowDate     :: m Date
  nowTime     :: m Time
  nowDateTime :: m DateTime

-------------------------------------------------------------------------------

instance nowHalogenM :: Now m => Now (HalogenM st act slots msg m) where
  now         = lift now
  nowDate     = lift nowDate
  nowTime     = lift nowTime
  nowDateTime = lift nowDateTime

--- /////////////////////////////////////////////////////////////////////// ---
