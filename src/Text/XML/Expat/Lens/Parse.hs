{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Text.XML.Expat.Lens.Parse
-- Copyright   : (c) 2013, Joseph Abrahamson
-- License     : MIT
-- 
-- Maintainer  : me@jspha.com
-- Stability   : experimental
-- Portability : non-portable
-- 
-- XML Parsing 'Prisms' from Hexpat.
-- 
-- While @Hexpat@ offers lazy, incremental parsing and this can
-- improve performance, we must force the parse to completion in order
-- to provide a 'Prism', so the lazy parsing is not offered here.

module Text.XML.Expat.Lens.Parse (
  _Expat, _ExpatWithOptions
  ) where

import Control.Lens
import Text.XML.Expat.Format
import Text.XML.Expat.Tree
import qualified Data.ByteString as S

-- | Provides an '_Expat' parsing 'Prism' with access to the
-- 'ParsingOptions'.
_ExpatWithOptions ::
  (GenericXMLString tag, GenericXMLString text) =>
  ParseOptions tag text -> Prism' S.ByteString (NodeG [] tag text)
_ExpatWithOptions opts = prism format' (\s -> bimap (const s) id $ parse' opts s)
{-# INLINE _ExpatWithOptions #-}

-- | Strict parsing and formatting of XML via 'format'' and 'parse''.
_Expat ::
  (GenericXMLString tag, GenericXMLString text) =>
  Prism' S.ByteString (NodeG [] tag text)
_Expat = _ExpatWithOptions defaultParseOptions
{-# INLINE _Expat #-}
