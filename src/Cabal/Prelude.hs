{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE Trustworthy      #-}
{-# LANGUAGE TypeOperators    #-}

#ifdef MIN_VERSION_base
#define MINVER_base_411 MIN_VERSION_base(4,11,0)
#define MINVER_base_48 MIN_VERSION_base(4,8,0)
#define MINVER_base_47 MIN_VERSION_base(4,7,0)
#else
#define MINVER_base_411 (__GLASGOW_HASKELL__ >= 804)
#define MINVER_base_48 (__GLASGOW_HASKELL__ >= 710)
#define MINVER_base_47 (__GLASGOW_HASKELL__ >= 708)
#endif
-- Code copied or derived from Distribution.Compat.Prelude.
-- | This module does two things:
--
-- * Acts as a compatibility layer, like @base-compat@.
--
-- * Provides commonly used imports.
module Cabal.Prelude (
    -- * Prelude
    --
    -- Prelude is re-exported, following is hidden:
    module BasePrelude,

#if !MINVER_base_48
    -- * base 4.8 shim
    Applicative(..), (<$), (<$>),
    Monoid(..),
#endif

    -- * Common type-classes
    Semigroup (..),
    gmappend, gmempty,
    Typeable, TypeRep, typeRep,
    Data,
    Generic,
    -- NFData (..), genericRnf,
    Binary (..),
    Structured,
    Alternative (..),
    MonadPlus (..),
    IsString (..),

    -- * Some types
    Map,
    Set,
    NonEmptySet,
    Identity (..),
    Proxy (..),
    Const (..),
    Void,

    -- * Data.Either
    partitionEithers,

    -- * Data.Maybe
    catMaybes, mapMaybe,
    fromMaybe,
    maybeToList, listToMaybe,
    isNothing, isJust,

    -- * Data.List
    unfoldr,
    isPrefixOf, isSuffixOf,
    intercalate, intersperse,
    sort, sortBy,
    nub, nubBy,
    partition,

    -- * Data.List.NonEmpty
    NonEmpty((:|)), nonEmpty, foldl1, foldr1,
    head, tail, last, init,

    -- * Data.Foldable
    Foldable, foldMap, foldr,
    null, length,
    find, foldl',
    traverse_, for_,
    any, all,
    toList,

    -- * Data.Traversable
    Traversable, traverse, sequenceA,
    for,

    -- * Data.Function
    on,

    -- * Data.Ord
    comparing,

    -- * Control.Arrow
    first,

    -- * Control.Monad
    liftM, liftM2,
    unless, when,
    ap, void,
    foldM, filterM,
    join, guard,

    -- * Control.Exception
    catch, throwIO, evaluate,
    Exception (..), IOException, SomeException (..),
#if !MINVER_base_48
    displayException,
#endif
    tryIO, catchIO, catchExit,

    -- * Data.Char
    isSpace, isDigit, isUpper, isAlpha, isAlphaNum,
    chr, ord,
    toLower, toUpper,

    -- * Data.Void
    absurd, vacuous,

    -- * Data.Word & Data.Int
    Word,
    Word8, Word16, Word32, Word64,
    Int8, Int16, Int32, Int64,

    -- * System.Exit
    ExitCode (..),
    exitWith, exitSuccess, exitFailure,

    -- * Text.Read
    readMaybe,

    -- * Debug.Trace (as deprecated functions)
    trace, traceShow, traceShowId,
    ) where

-- We also could hide few partial function
import Prelude                       as BasePrelude hiding
  ( mapM, mapM_, sequence, null, length, foldr, any, all, head, tail, last, init
  -- partial functions
  , read
  , foldr1, foldl1
#if MINVER_base_411
  -- As of base 4.11.0.0 Prelude exports part of Semigroup(..).
  -- Hide this so we instead rely on Distribution.Compat.Semigroup.
  , Semigroup(..)
#endif
#if MINVER_base_48
  , Word
  -- We hide them, as we import only some members
  , Traversable, traverse, sequenceA
  , Foldable, foldMap
#endif
  )

-- AMP
#if !MINVER_base_48
import Control.Applicative           (Applicative (..), (<$), (<$>))
import Data.Foldable                 (toList)
import Distribution.Compat.Semigroup (Monoid (..))
#else
import Data.Foldable (Foldable (toList), length, null)
#endif

import Data.Foldable    (Foldable (foldMap, foldr), all, any, find, foldl', for_, traverse_)
import Data.Traversable (Traversable (sequenceA, traverse), for)

import qualified Data.Foldable

-- Extra exports
import Control.Applicative           (Alternative (..))
import Control.Applicative           (Const (..))
import Control.Arrow                 (first)
-- import Control.DeepSeq               (NFData (..), deepseq, force)
import Control.Exception             (Exception (..), IOException, SomeException (..), catch, evaluate, throwIO)
import Control.Monad                 (MonadPlus (..), ap, filterM, foldM, guard, join, liftM, liftM2, unless, void, when)
import Data.Char                     (chr, isAlpha, isAlphaNum, isDigit, isSpace, isUpper, ord, toLower, toUpper)
import Data.Data                     (Data)
import Data.Either                   (partitionEithers)
import Data.Function                 (on)
import Data.Functor.Identity         (Identity (..))
import Data.Int                      (Int16, Int32, Int64, Int8)
import Data.List                     (intercalate, intersperse, isPrefixOf, isSuffixOf, nub, nubBy, partition, sort, sortBy, unfoldr)
import Data.List.NonEmpty            (NonEmpty ((:|)), nonEmpty, head, init, last, tail)
import Data.Map                      (Map)
import Data.Maybe                    (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord                      (comparing)
import Data.Proxy                    (Proxy (..))
import Data.Set                      (Set)
import Data.String                   (IsString (..))
import Data.Void                     (Void, absurd, vacuous)
import Data.Word                     (Word, Word16, Word32, Word64, Word8)
import Distribution.Compat.Binary    (Binary (..))
import Distribution.Compat.Semigroup (Semigroup (..), gmappend, gmempty)
import Distribution.Compat.Typeable  (TypeRep, Typeable, typeRep)
import GHC.Generics                  (Generic)
import System.Exit                   (ExitCode (..), exitFailure, exitSuccess, exitWith)
import Text.Read                     (readMaybe)

-- import qualified Text.PrettyPrint as Disp

import Distribution.Compat.Exception
import Distribution.Compat.NonEmptySet (NonEmptySet)
import Distribution.Utils.Structured   (Structured)

import qualified Debug.Trace

#if !MINVER_base_48
-- | Test whether the structure is empty. The default implementation is
-- optimized for structures that are similar to cons-lists, because there
-- is no general way to do better.
null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

-- | Returns the size/length of a finite structure as an 'Int'.  The
-- default implementation is optimized for structures that are similar to
-- cons-lists, because there is no general way to do better.
length :: Foldable t => t a -> Int
length = foldl' (\c _ -> c+1) 0
#endif


{-# INLINE foldr1 #-}
foldr1 :: (a -> a -> a) -> NonEmpty a -> a
foldr1 = Data.Foldable.foldr1

{-# INLINE foldl1 #-}
foldl1 :: (a -> a -> a) -> NonEmpty a -> a
foldl1 = Data.Foldable.foldl1

-------------------------------------------------------------------------------
-- Trace
-------------------------------------------------------------------------------

-- Functions from Debug.Trace
-- but with DEPRECATED pragma, so -Werror will scream on them.

trace :: String -> a -> a
trace = Debug.Trace.trace
{-# DEPRECATED trace "Don't leave me in the code" #-}

traceShowId :: Show a => a -> a
traceShowId x = Debug.Trace.traceShow x x
{-# DEPRECATED traceShowId "Don't leave me in the code" #-}

traceShow :: Show a => a -> b -> b
traceShow = Debug.Trace.traceShow
{-# DEPRECATED traceShow "Don't leave me in the code" #-}