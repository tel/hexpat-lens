
-- |
-- Module      : Text.XML.Expat.Lens.Generic
-- Copyright   : (c) 2013, Joseph Abrahamson
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- A Hexpat lens module for generic tags.
-- 
-- Lenses provide power to do very concise XML tree diving. This
-- module provides a less general interface to the Hexpat datatypes
-- via lenses.

{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Text.XML.Expat.Lens.Generic (

  -- * Basic inspection
  name, attributes, text,

  -- * Recursive inspection
  children, allNodes, (./),

  -- * Filters
  named, parameterized
         
  ) where

import Control.Applicative
import Control.Lens hiding (children)
import Control.Monad

import Text.XML.Expat.Tree

-- | Traverses the name of an 'Element'. This is as
-- an "Affine", or 0-or-1 target, 'Traversal'. In regex terms, you
-- can think of it like the @?@ suffix modifier.

name :: Traversal' (NodeG f tag text) tag
name inj (Element n a c) = (\n' -> Element n' a c) <$> inj n
name _   t               = pure t
{-# INLINE name #-}

-- | Traverses to the list of attributes of an 'Element'. This is as
-- an "Affine", or 0-or-1 target, 'Traversal'. In regex terms, you
-- can think of it like the @?@ suffix modifier.

attributes :: Traversal' (NodeG f tag text) (Attributes tag text)
attributes inj (Element n a c) = (\a' -> Element n a' c) <$> inj a
attributes _   t               = pure t
{-# INLINE attributes #-}

-- The @attributes@ form, effectively, a lookup table allowing us to
-- instantiate @At@. Then, we get @Ixed@, @Each@, and @Contains@ for
-- "free".

type instance Index   (NodeG f tag text) = tag
type instance IxValue (NodeG f tag text) = text

-- | This forms a valid 'At' instance under the assumption that
-- there are no repeated keys in the 'Attributes' list. Since
-- @hexpat@ won't parse invalid XML this holds after parsing, so
-- this 'At' instance is valid so long as the invariants aren't
-- subverted in some other way, such as by modify the 'Attributes'
-- list directly via the 'attributes' 'Traversal'.

instance Eq tag => At (NodeG f tag text) where
  at k f e =
    indexed f k (join (e ^? attributes . to (lookup k)))
    <&> \r -> e & attributes %~ ins k r
    where
      ins _   Nothing    [] = []
      ins key (Just res) [] = [(key, res)]
      ins key mayRes ((key', res'):rest)
        | key == key' = case mayRes of
          Nothing  -> rest
          Just res -> (key, res):rest
        | otherwise   = (key', res') : ins key mayRes rest
      {-# INLINE ins #-}
  {-# INLINE at #-}

instance (Eq tag) => Ixed (NodeG c tag text) where
   ix = ixAt
   
instance Traversable f => Plated (NodeG f tag text) where
  plate = children . traverse
  {-# INLINE plate #-}

-- | Traverses the children of an 'Element'. This is as
-- an "Affine", or 0-or-1 target, 'Traversal'. In regex terms, you
-- can think of it like the @?@ suffix modifier.

children :: Traversal' (NodeG f tag text) (f (NodeG f tag text))
children inj (Element n a c) = Element n a <$> inj c
children _   t               = pure t
{-# INLINE children #-}

-- | Prismatic access to the text of a 'Text' node. This is more
-- powerful than 'name', 'children', and 'attributes' since it can
-- be 'Review'ed.

text :: Prism' (NodeG f tag text) text
text = dimap go come . right' where
  go e@Element{} = Left e
  go (Text t)    = Right t
  {-# INLINE go #-}
  come (Left it) = pure it
  come (Right t) = Text <$> t
  {-# INLINE come #-}
{-# INLINE text #-}

-- We can use plated/uniplate lenses to traverse all of the elements of
-- the tree in a bottom up fashion.

-- | Produces a list of all 'UNode's in a XML tree. Synonym for
-- 'universe'.

allNodes :: Traversable c => NodeG c tag text -> [NodeG c tag text]
allNodes = universe
{-# INLINE allNodes #-}

-- And if we build one sort-of @Traversal@ then we'll have replicated
-- almost all of the functionality of @NodeClass@ in lenses. This uses
-- 'Control.Lens.Fold.filtered' so the caveats there apply.

-- | Traverses 'Element's which have a particular name.
named
  :: (Eq a, Applicative f, Choice p) =>
     a -> Optic' p f (NodeG f1 a text) (NodeG f1 a text)
named n = filtered $ maybe False (== n) . preview name
{-# INLINE named #-}

-- | @parameterized k v@ traverses 'Element's which match the value
-- @v@ at the key @k@ in their attributes.
parameterized
  :: (Eq (IxValue a), Applicative f, Choice p, Ixed a) =>
     Index a -> IxValue a -> Optic' p f a a
parameterized k v = filtered check where
  check u = case u ^? ix k . to (==v) of
    Just True -> True
    _         -> False
  {-# INLINE check #-}
{-# INLINE parameterized #-}


-- | Glue two 'Traversal's together as relations. This is much like
-- @XPath@'s *slash*.

(./) :: Plated i => Traversal' s i -> Traversal' i a -> Traversal' s a
l ./ m = l . plate . m
