
-- |
-- Module      : Text.XML.Expat.Lens.Unqualified
-- Copyright   : (c) 2013, Joseph Abrahamson
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- A simple Hexpat lens module. This is nothing more than a
-- type-specialized re-export of "Text.XML.Expat.Lens.Generic".
-- 
-- Lenses provide power to do very concise XML tree diving. This
-- module provides a less general interface to the Hexpat datatypes
-- via lenses.

{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.XML.Expat.Lens.Unqualified (

  -- * Basic inspection
  name, attributes, text,

  -- * Recursive inspection
  children, allNodes,

  -- * Filters
  named, parameterized
  
  ) where

import Control.Applicative
import Control.Lens hiding (children)
import Text.XML.Expat.Tree
import qualified Text.XML.Expat.Lens.Generic as G

name :: Traversal' (UNode t) t
name = G.name
{-# INLINE name #-}

attributes :: Traversal' (UNode t) (UAttributes t)
attributes = G.attributes
{-# INLINE attributes #-}

children :: Traversal' (UNode t) [UNode t]
children = G.children
{-# INLINE children #-}

text :: Prism' (UNode t) t
text = G.text
{-# INLINE text #-}

allNodes :: UNode t -> [UNode t]
allNodes = G.allNodes
{-# INLINE allNodes #-}

named :: (Choice p, Applicative f, Eq t) => t -> Overloaded' p f (UNode t) (UNode t)
named = G.named
{-# INLINE named #-}

parameterized :: (Choice p, Applicative f, Eq t, GenericXMLString t) =>
                 t -> t -> Overloaded' p f (UNode t) (UNode t)
parameterized = G.parameterized
{-# INLINE parameterized #-}
