
-- |
-- Module      : Text.XML.Expat.Lens.Names
-- Copyright   : (c) 2013, Joseph Abrahamson
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- Isos on 'QName's and 'NName's.
-- 
-- Lenses will provide the power to do very concise XML tree
-- diving. This module provides a less general interface to the Hexpat
-- datatypes via lenses.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.XML.Expat.Lens.Names (

  prefix, namespace, qualified, namespaced,
  HasLocalPart (..)
  
  ) where

import Control.Applicative
import Control.Exception
import Control.Lens hiding (children)

import Text.XML.Expat.Tree

import Control.DeepSeq
import System.IO.Unsafe

class HasLocalPart a where
  localPart :: Lens' (a t) t

prefix :: Lens' (QName text) (Maybe text)
prefix inj (QName pref part) = (\pref' -> QName pref' part) <$> inj pref
{-# INLINE prefix #-}

instance HasLocalPart QName where
  localPart inj (QName pref part) = (\part' -> QName pref part') <$> inj part
  {-# INLINE localPart #-}

namespace :: Lens' (NName text) (Maybe text)
namespace inj (NName ns part) = (\ns' -> NName ns' part) <$> inj ns
{-# INLINE namespace #-}

instance HasLocalPart NName where
  localPart inj (NName ns part) = (\part' -> NName ns part') <$> inj part
  {-# INLINE localPart #-}

-- | Iso between a node marked by a "stringy" name to one using a
-- qualified 'QName'.
qualified ::
  (GenericXMLString text, NodeClass n c) =>
  Iso' (n c text text) (n c (QName text) text) 
qualified = iso toQualified fromQualified

-- | 'Prism' between a node marked by a qualified 'QName' name to one
-- using a namespaced 'NName'. Normally this throws an exception if
-- the namespace is non-standard, but here the 'Prism' simply fails if
-- incompatible.
namespaced ::
  ( GenericXMLString text, NodeClass n c
  , Show text, Ord text, NFData (n c (NName text) text) ) =>
  Prism' (n c (QName text) text)
         (n c (NName text) text) 
namespaced = prism' fromNamespaced (muspoon . toNamespaced)

-- | It's tragic that this exists. Stop 'error's before they start!
muspoon :: NFData a => a -> Maybe a
muspoon a = unsafePerformIO $ deepseq a (Just `fmap` return a) `catches` handles
  where handles = [ Handler $ \(_ :: ErrorCall)    -> return Nothing ]
      

