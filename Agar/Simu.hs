-- |
-- Module:     Agar.Simu
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module Agar.Simu
    ( -- * Reexports
      module Agar.Simu.Core,
      module Agar.Simu.Entities,

      -- * External
      module Prelude,
      module Control.Wire
    )
    where

import Prelude hiding ((.), id, until)
import Control.Wire
import Agar.Simu.Core
import Agar.Simu.Entities
