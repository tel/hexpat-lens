
-- |
-- Module      : Text.XML.Expat.Lens.Unqualified
-- Copyright   : (c) 2013, Joseph Abrahamson
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- A simple Hexpat lens module.
-- 
-- Lenses provide power to do very concise XML tree diving. This
-- module provides a less general interface to the Hexpat datatypes
-- via lenses.

{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Text.XML.Expat.Lens.Unqualified where

import Control.Applicative
import Control.Lens hiding (children)

import Text.XML.Expat.Tree

-- | Traverses the name of an 'Element'. This is as
-- an "Affine", or 0-or-1 target, 'Traversal'. In regex terms, you
-- can think of it like the @?@ suffix modifier.

name :: Traversal' (UNode t) t
name inj (Element n a c) = (\n' -> Element n' a c) <$> inj n
name _   t               = pure t
{-# INLINE name #-}

-- | Traverses to the list of attributes of an 'Element'. This is as
-- an "Affine", or 0-or-1 target, 'Traversal'. In regex terms, you
-- can think of it like the @?@ suffix modifier.

attributes :: Traversal' (UNode t) (UAttributes t)
attributes inj (Element n a c) = (\a' -> Element n a' c) <$> inj a
attributes _   t               = pure t
{-# INLINE attributes #-}

-- The @attributes@ form, effectively, a lookup table allowing us to
-- instantiate @At@. Then, we get @Ixed@, @Each@, and @Contains@ for
-- "free".

type instance Index   (UNode a) = a
type instance IxValue (UNode a) = a

-- | This forms a valid 'At' instance under the assumption that
-- there are no repeated keys in the 'Attributes' list. Since
-- @hexpat@ won't parse invalid XML this holds after parsing, so
-- this 'At' instance is valid so long as the invariants aren't
-- subverted in some other way, such as by modify the 'Attributes'
-- list directly via the 'attributes' 'Traversal'.

instance (GenericXMLString a) => At (UNode a) where
  at k f e = indexed f k (getAttribute e k) <&> \r -> alterAttribute k r e

instance (GenericXMLString a, Applicative f) => Ixed f (UNode a) where
  ix = ixAt
  
instance ( GenericXMLString a
         , Applicative f
         , Contravariant f ) => Contains f (UNode a) where
  contains = containsAt

-- | Traverses the children of an 'Element'. This is as
-- an "Affine", or 0-or-1 target, 'Traversal'. In regex terms, you
-- can think of it like the @?@ suffix modifier.

children :: Traversal' (UNode t) [UNode t]
children inj (Element n a c) = (\c' -> Element n a c') <$> inj c
children _   t               = pure t
{-# INLINE children #-}

-- | Prismatic access to the text of a 'Text' node. This is more
-- powerful than 'name', 'children', and 'attributes' since it can
-- be 'Review'ed.

text :: Prism' (UNode t) t
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

-- | Produces a list of all 'UNode's in a XML tree.

allNodes :: UNode t -> [UNode t]
allNodes = universeOf (children . traverse)

-- And if we build one sort-of @Traversal@ then we'll have replicated
-- almost all of the functionality of @NodeClass@ in lenses. This uses
-- 'Control.Lens.Fold.filtered' so the caveats there apply.

-- | Traverses 'Element's which have a particular name.

named :: (Choice p, Applicative f, Eq t) => t -> Overloaded' p f (UNode t) (UNode t)
named n = filtered (isNamed n)

-- | @parameterized k v@ traverses 'Element's which match the value
-- @v@ at the key @k@ in their attributes.

parameterized :: (Choice p, Applicative f, Eq t, GenericXMLString t) =>
                 t -> t -> Overloaded' p f (UNode t) (UNode t)
parameterized k v = filtered check where
  check u = case u ^? ix k . to (==v) of
    Just True -> True
    _         -> False
  {-# INLINE check #-}
{-# INLINE parameterized #-}
