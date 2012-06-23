{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Trans.UnionFind
  ( UnionFindT, runUnionFind
  , Point, fresh, repr, descriptor, union, equivalent
  ) where

import Control.Applicative (Applicative)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Data.UnionFind.IntMap (Point)
import qualified Control.Monad.Trans.State as State
import qualified Data.UnionFind.IntMap as UF

-- | A monad transformer that adds union find operations.
--
-- The @p@ parameter is the type of points.  Uses the
-- "Data.UnionFind.IntMap" as the underlying union-find
-- implementation.
newtype UnionFindT p m a = UnionFindT {
  unUnionFindT :: StateT (UF.PointSupply p) m a
  } deriving (Functor, Applicative, Monad, MonadTrans)

runUnionFind :: Monad m => UnionFindT p m a -> m a
runUnionFind = (`evalStateT` UF.newPointSupply) . unUnionFindT

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- | Create a new point with the given descriptor.  The returned is
-- only equivalent to itself.
--
-- Note that a 'Point' has its own identity.  That is, if two points
-- are equivalent then their descriptors are equal, but not vice
-- versa.
--
fresh :: Monad m => p -> UnionFindT p m (Point p)
fresh x = UnionFindT . StateT $ return . swap . flip UF.fresh x

-- | /O(1)/. @repr point@ returns the representative point of
-- @point@'s equivalence class.
repr :: Monad m => Point p -> UnionFindT p m (Point p)
repr = UnionFindT . State.gets . flip UF.repr

-- | Return the descriptor of the 
descriptor :: Monad m => Point p -> UnionFindT p m p
descriptor = UnionFindT . State.gets . flip UF.descriptor

-- | Join the equivalence classes of the points.  The resulting
-- equivalence class will get the descriptor of the second argument.
union :: Monad m => Point p -> Point p -> UnionFindT p m ()
union p1 p2 = UnionFindT . State.modify $ \x -> UF.union x p1 p2

-- | Test if the two elements are in the same equivalence class.
-- 
-- @
-- liftA2 (==) (repr x) (repr y)
-- @
equivalent :: Monad m => Point p -> Point p -> UnionFindT p m Bool
equivalent p1 p2 = UnionFindT . State.gets $ \x -> UF.equivalent x p1 p2
