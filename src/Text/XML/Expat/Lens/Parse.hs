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
-- XML parsing 'Prism's from Hexpat. HTML parsing 'Iso's from TagSoup.
-- 
-- While @Hexpat@ offers lazy, incremental parsing and this can
-- improve performance, we must force the parse to completion in order
-- to provide a 'Prism', so the lazy parsing is not offered here.

module Text.XML.Expat.Lens.Parse (
  _XML, _XMLWithOptions, _HTML, _HTML', _HTMLWithOptions
  ) where

import Control.Lens
import Text.XML.Expat.Format
import Text.XML.Expat.Tree       as T
import Text.XML.Expat.TagSoup    as TS
import qualified Data.ByteString as S

-- | Strict parsing and formatting of XML via 'format'' and 'parse''.
_XML ::
  (GenericXMLString tag, GenericXMLString text) =>
  Prism' S.ByteString (NodeG [] tag text)
_XML = _XMLWithOptions defaultParseOptions
{-# INLINE _XML #-}

-- | Provides an '_XMLWithOptions parsing 'Prism' with access to the
-- 'ParsingOptions'.
_XMLWithOptions ::
  (GenericXMLString tag, GenericXMLString text) =>
  T.ParseOptions tag text -> Prism' S.ByteString (NodeG [] tag text)
_XMLWithOptions opts = prism format' (\s -> bimap (const s) id $ parse' opts s)
{-# INLINE _XMLWithOptions #-}

-- | Uses "tag soup" parsing to build a 'UNode' tree. Technically a
-- retract, since @_HTML@ tries very hard to return *something*, we
-- get an 'Iso' instead of a 'Prism'.
-- 
-- prop> view (from _HTML . _HTML) = id
-- 
_HTML
  :: (GenericXMLString text) => Iso' S.ByteString (UNode text)
_HTML = _HTMLWithOptions TS.parseOptions
{-# INLINE _HTML #-}

-- | Uses "tag soup" parsing to build a 'UNode' tree. Technically a
-- retract, since @_HTML@ tries very hard to return *something*, we
-- get an 'Iso' instead of a 'Prism'. Uses the *fast* tag soup parsing
-- options.
-- 
-- prop> view (from _HTML' . _HTML') = id
-- 
_HTML'
  :: (GenericXMLString text) => Iso' S.ByteString (UNode text)
_HTML' = _HTMLWithOptions TS.parseOptions
{-# INLINE _HTML' #-}

-- | Like '_HTML but allows choice of 'TS.ParseOptions'.
_HTMLWithOptions
  :: (GenericXMLString text) =>
     TS.ParseOptions S.ByteString -> Iso' S.ByteString (UNode text)
_HTMLWithOptions opts = iso (parseTagsOptions opts) format'
{-# INLINE _HTMLWithOptions #-}
