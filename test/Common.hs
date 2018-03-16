{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Common where

import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

-- | Model of a call to a Union Find implementation.
--   An integer n in any of the value constructors refer
--   to the n'th created point.
data Call = Fresh Int | Union Int Int
             deriving (Show, Eq)

-- |Representation of a list of 'Call' values: this identifies a sequence of n
--  consecutive operations, such as e.g. Fresh 0>Fresh 1>Union 0 1, that will
--  result in a context with up to n sets once executed by the run function.
newtype CallList = CL [Call] deriving (Show, Eq)

-- Generators

instance Arbitrary CallList where
  arbitrary :: Gen CallList
  arbitrary = simple
  shrink :: CallList -> [CallList]
  shrink (CL cs) = removePoint    cs ++
                  removeNonFresh cs ++
                  sortFreshFirst cs

-- |Generates a 'CallList' in a meaningful way, i.e. avoiding calling
--  operations before creating points with 'Fresh' and keeping track of the
--  points already created.
simple :: Gen CallList
simple = do
  cl <- s' 0
  return $ CL cl
  where
    s' :: Int -- Points added so far
          -> Gen [Call]
    s' n_points = sized $ \remaining -> case () of
     _| remaining <= 0 -> return []
      | otherwise -> do
        nonFreshCalls <- choose (0,n_points `div` 2) -- TODO should probably grow smarter
        listOfNonFresh <- vectorOf nonFreshCalls $ 
                              do n1 <- choose (0, n_points)
                                 n2 <- choose (0, n_points)
                                 return (Union n1 n2)
        rec <- resize (max 0 (remaining-nonFreshCalls-1)) $ s' (n_points+1)
        return $ (Fresh n_points):(listOfNonFresh++rec)


-- Shrinking

-- | For each fresh call,
--     return one [Call] missing it and all other calls referencing that point.
--     Decrements the indexes of points bigger than the one removed.
removePoint :: [Call] -> [CallList]
removePoint cs =
  let n_fresh = numberOfFresh cs
  in map
      (\id -> CL $ restoreIdGap id $ filterPoint id cs)
      [0..(n_fresh-1)]
  where
    filterPoint :: Int -> [Call] -> [Call]
    filterPoint id = filter $ predC (/=id)
    restoreIdGap :: Int -> [Call] -> [Call]
    restoreIdGap id = map $ mapC (\p -> if p>id then p-1 else p)

-- | For each non-fresh call,
--     remove one [Call] missing it
removeNonFresh :: [Call] -> [CallList]
removeNonFresh cs =
  let n = length cs
  in map CL $ filter (/=[]) $ map
      (\i -> removeIfNotFresh i cs)
      [0..(n-1)]
  where
    removeIfNotFresh :: Int -> [Call] -> [Call]
    removeIfNotFresh i cs | isFresh (cs!!i) = []
                          | otherwise = (take i cs) ++ (drop (i+1) cs)

-- | If not all fresh calls are first,
--     return one [Call] where they are
sortFreshFirst :: [Call] -> [CallList]
sortFreshFirst cs =
  if allFreshFirst cs then [] else [CL (sort cs)]
  where
    sort :: [Call] -> [Call]
    sort cs = filter isFresh cs ++ (filter (not . isFresh) cs)

-- Helpers

-- | Map the internal id's of the calls
mapC :: (Int -> Int) -> Call -> Call
mapC f c = case c of
  Fresh i     -> Fresh (f i)
  Union i1 i2 -> Union (f i1) (f i2)

-- |'mapC' extended to a list of 'Call's.
mapCs :: (Int -> Int) -> [Call] -> [Call]
mapCs f = map (mapC f)

-- |Applies a given function to the index enclosed in a 'Call'. Internal use.
predC :: (Int -> Bool) -> Call -> Bool
predC p c = case c of
  Fresh i     -> p i
  Union i1 i2 -> (p i1) && (p i2)

-- |Returns 'True' if the input call is a 'Fresh'.
isFresh :: Call -> Bool
isFresh c = case c of
  Fresh _ -> True
  _       -> False

-- |Returns 'True' if all the 'Fresh' calls are at the start of the list.
allFreshFirst :: [Call] -> Bool
allFreshFirst = null . dropWhile (not.isFresh) . dropWhile isFresh

-- |Returns the number of 'Fresh' calls inside a list.
numberOfFresh :: [Call] -> Int
numberOfFresh = length . filter isFresh


-- Tests

-- |Checks if all indices in the given 'CallList' are meaningful
--  (i.e. non negative, within the number of existing points).
prop_refsInRange :: CallList -> Property
prop_refsInRange (CL cs) = aboveZero cs .&&. belowExisting cs

-- |Checks if 'prop_refsInRange' is holds for shrinked CallLists.
prop_refsInRange_shrink :: CallList -> Property
prop_refsInRange_shrink cl =
  let shrinks = shrink cl
  in conjoin $ map prop_refsInRange shrinks

aboveZero :: [Call] -> Property
aboveZero cs =
  counterexample (show cs) (property $ and $ map (predC (>=0)) cs)

belowExisting :: [Call] -> Property
belowExisting cs =
  counterexample (show cs) (property $ internal cs 0)
  where
    internal cs n = case cs of
      []            -> True
      (Fresh _:cs') -> internal cs' (n+1)
      (c:cs')       -> predC (<n) c && internal cs' n

-- |Checks that the 'Fresh' calls are not invoked out of order, e.g.
--  [Fresh 1,Fresh 3,Union 1 3,Fresh 2,Union 1 3] will result in an
--  error since Fresh 3 can't be called before Fresh 2.
prop_pointsContinious :: CallList -> Property
prop_pointsContinious (CL cs) =
  counterexample (show cs) (property $ internal (filter isFresh cs) 0)
  where
  internal cs n = case cs of
    []              -> True
    (Fresh i : cs') -> i==n && internal cs' (n+1)

-- |Checks if 'prop_pointsContinious' is holds for shrinked CallLists.
prop_pointsContinious_shrink :: CallList -> Property
prop_pointsContinious_shrink cl =
  let shrinks = shrink cl
  in conjoin $ map prop_pointsContinious shrinks

