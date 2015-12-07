-- |
-- Module:     Agar.Simu
-- Copyright:  (c) 2015 Martin Villagra
-- License:    BSD3
-- Maintainer: Martin Villagra <mvillagra0@gmail.com>

module AgarSimu
    ( -- * Reexports
      module AgarSimu.Core,
      module AgarSimu.Entities,

      -- * External
      module Prelude,
      module Control.Wire
    )
    where

import Prelude hiding ((.), id, until)
import Control.Wire
import AgarSimu.Core
import AgarSimu.Entities
