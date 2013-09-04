
-- |
-- Module      : Text.XML.Expat.Lens
-- Copyright   : (c) 2013, Joseph Abrahamson
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- A simple Hexpat lens module. Right now this only re-exports lenses
-- on Hexpat 'UName's. In the future it may be better to export the
-- general API in this module.

module Text.XML.Expat.Lens (

  module Text.XML.Expat.Lens.Unqualified,
  module Text.XML.Expat.Lens.Parse
  
  ) where

import Text.XML.Expat.Tree
import Text.XML.Expat.Lens.Unqualified
import Text.XML.Expat.Lens.Parse
