-- | A tiny library for composing indexed maps, folds and traversals.
--
-- One of the benefits of lenses and traversals is that they can be
-- created, composed and used, using only the machinery available in base.
-- For more advanced use cases, there is the @lens@ library.
--
-- This library tries to provide something similar for indexed traversals.
--
-- Many data structures provide functions which map or traverse while providing
-- access to an index. For example, @containers@ provides @mapWithKey@ and
-- @traverseWithKey@ functions for @Map@. Using this module, it is possible
-- to compose such maps and traversals, while combining indices using some
-- @Monoid@.
--
-- To use this library, first import @Control.Category@, since you will need
-- the more general version of composition defined in that module.
--
-- Next, wrap any maps or traversals you wish to use with the 'WithIndex'
-- constructor. You may also need to change the index type using the 'reindex'
-- function. These wrapped functions can be composed using the (@Category@)
-- composition operator.
--
-- Regular maps and traversals can also be used, via the 'withoutIndex' function.

module Data.WithIndex
  ( WithIndex(..)
  , reindex
  , withoutIndex
  ) where

import Prelude hiding (id, (.))
import Control.Category (Category(..))

-- | A wrapper for a mapping or traversal function which uses an index.
--
-- For example, using the @containers@ library:
--
-- @
--  WithIndex mapWithKey
--    :: WithIndex i (a -> b) (Map i a -> Map i b)
--  WithIndex foldMapWithKey
--    :: Monoid m => WithIndex i (a -> m) (Map i a -> m)
--  WithIndex traverseWithKey
--    :: Applicative t => WithIndex i (a -> t b) (Map i a -> t (Map i b))
-- @
--
-- These wrapped functions can be composed using the (@Category@) composition
-- operator:
--
-- @
--  WithIndex mapWithKey . WithIndex mapWithKey
--    :: Monoid i =>
--       WithIndex i (a -> b) (Map i (Map i a) -> Map i (Map i b))
-- @
--
-- and then applied using 'withIndex':
--
-- @
--  withIndex $ WithIndex mapWithKey . WithIndex mapWithKey
--    :: Monoid i => (i -> a -> b) -> Map i (Map i a) -> Map i (Map i b)
-- @
newtype WithIndex i a b = WithIndex { withIndex :: (i -> a) -> b }

instance Monoid i => Category (WithIndex i) where
  id = WithIndex $ \i -> i mempty
  WithIndex f . WithIndex g = WithIndex $ \b -> f $ \i1 -> g $ \i2 -> b (mappend i1 i2)

-- | Change the @Monoid@ used to combine indices.
--
-- For example, to keep track of only the first index seen, use @Data.Monoid.First@:
--
-- @
--  reindex (First . pure)
--    :: WithIndex i a b -> WithIndex (First i) a b
-- @
--
-- or keep track of all indices using a list
--
-- @
--  reindex (: [])
--    :: WithIndex i a b -> WithIndex [i] a b
-- @
reindex :: (i -> j) -> WithIndex i a b -> WithIndex j a b
reindex ij (WithIndex f) = WithIndex $ \a -> f (a . ij)

-- | Turn a regular function into an wrapped function, so that it can be
-- composed with other wrapped functions.
--
-- For example, to traverse two layers, keeping only the first index:
--
-- @
--  WithIndex mapWithKey . withoutIndex Data.Map.map
--    :: Monoid i =>
--       WithIndex i (a -> b) (Map i (Map k a) -> Map i (Map k b))
-- @
withoutIndex :: Monoid i => (a -> b) -> WithIndex i a b
withoutIndex f = WithIndex $ \a -> f (a mempty)
