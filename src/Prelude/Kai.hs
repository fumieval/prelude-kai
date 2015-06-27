{-# LANGUAGE CPP #-}
module Prelude.Kai (

    -- * Standard types, classes and related functions

    -- ** Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise, bool,

    Void,
    absurd, vacuous,

    Identity(..),

    Proxy(..),
    asProxyTypeOf,

    Maybe(Nothing, Just),
    maybe,

    Either(Left, Right),
    either,

    Ordering(LT, EQ, GT),
    Char, String,

    -- *** Tuples
    fst, snd, curry, uncurry, swap,

    -- ** Basic type classes
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- ** Numbers

    -- *** Numeric types
    Int, Integer, Float, Double,
    Rational, Word,

    -- *** Numeric type classes
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- *** Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,

    -- ** Monoids
    Monoid(mempty, mappend, mconcat),
    (<>),

    -- ** Monads and functors
    Functor(fmap, (<$)), (<$>),
    Applicative(pure, (<*>), (*>), (<*)),
    forever,
    liftA2,
    replicateA,
    replicateA_,
    Alternative(empty, (<|>), some, many),
    optional,
    Monad((>>=), (>>), return, fail),
    mapM_, sequence_, (=<<),
    void, when, unless,

    -- ** Folds and traversals
    Foldable(fold,   -- :: Monoid m => t m -> m
             foldMap,   -- :: Monoid m => (a -> m) -> t a -> m
             foldr,     -- :: (a -> b -> b) -> b -> t a -> b
             foldr', -- :: (a -> b -> b) -> b -> t a -> b
             foldl,     -- :: (b -> a -> b) -> b -> t a -> b
             foldl', -- :: (b -> a -> b) -> b -> t a -> b
             toList,
             null,
             length,
             elem,
             maximum,   -- :: (Foldable t, Ord a) => t a -> a
             minimum,   -- :: (Foldable t, Ord a) => t a -> a
             product,   -- :: (Foldable t, Num a) => t a -> a
             sum),      -- :: Num a => t a -> a

    traverse_,
    for_,

    Traversable(traverse, sequenceA, mapM, sequence),

    for,

    -- ** Miscellaneous functions
    id, const, (.), flip, ($), (&), until,
    asTypeOf, error, undefined,
    seq, ($!),

    -- * List operations
    map, (++), filter,
    head, last, tail, init, null, length, (!!),
    reverse,
    -- *** Special folds
    and, or, any, all,
    concat, concatMap,
    -- ** Building lists
    -- *** Scans
    scanl, scanl1, scanr, scanr1,
    -- *** Infinite lists
    iterate, repeat, replicate, cycle,
    -- ** Sublists
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    -- ** Searching lists
    notElem, lookup,
    -- ** Zipping and unzipping lists
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
    -- ** Functions on strings
    lines, words, unlines, unwords,

    -- * Converting to and from @String@
    -- ** Converting to @String@
    ShowS,
    Show(showsPrec, showList, show),
    shows,
    showChar, showString, showParen,
    -- ** Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,

    -- * Basic Input and output
    IO,
    -- ** Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- *** Output functions
    putChar,
    putStr, putStrLn, print,
    -- *** Input functions
    getChar,
    getLine, getContents, interact,
    -- *** Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    -- ** Exception handling in the I\/O monad
    IOError, ioError, userError,

  ) where
import Prelude ()
import Control.Monad
import System.IO
import System.IO.Error
import Data.Bool
import Data.List
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import Data.Maybe
import Data.Proxy hiding (asProxyTypeOf)
import Data.Traversable
import Data.Tuple
import GHC.Base hiding ( foldr, mapM, sequence, id, (.) )
import Data.Function ((&), fix)
import Text.Read
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.Show
import Control.Category
import Control.Applicative
import Data.Semigroup
import Data.Tuple
import Data.Void
import Data.Orphans

-- | 'asProxyTypeOf' is a type-restricted version of 'const'.
-- It is usually used as an infix operator, and its typing forces its first
-- argument (which is usually overloaded) to have the same type as the tag
-- of the second.
asProxyTypeOf :: a -> proxy a -> a
asProxyTypeOf = const
{-# INLINE asProxyTypeOf #-}

#if !MIN_VERSION_base(4,9,0)
instance Monoid m => Monad ((,) m) where
    return a = (mempty, a)
    (m, a) >>= k = let (m', b) = k a in (mappend m m', b)
#endif

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA m f = go m where
    go 0 = pure []
    go n = liftA2 (:) f (go (n - 1))
{-# INLINE replicateA #-}

replicateA_ :: Applicative f => Int -> f a -> f ()
replicateA_ m f = go m where
    go 0 = pure ()
    go n = f *> go (n - 1)
{-# INLINE replicateA_ #-}