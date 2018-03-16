{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs     #-}

module IntMapProps where

import           Common
import qualified Data.List             as List
import           Data.Map              ((!))
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.UnionFind.IntMap
import           Test.QuickCheck

-- |'PointEnvironment' is a data type containing 'PointSupply' and a reference to all
--  'Point's created, plus a reference to the original 'CallList' for internal
--  use.
--  The enclosed 'PointSuppy' contains Points with descriptors 0..(nPoints-1).
--  This data type is needed as the user of Data.UnionFind.IntMap.PointSupply need to
--  keep track of the points created.
data PointEnvironment = PE {
  pointSupply    :: PointSupply Int,
  pointMap       :: Map.Map Int (Point Int),
  nPoints        :: Int,
  sourceCallList :: CallList
}

-- |Custom 'Show' instance because no 'Show' is derived by 'Point's in IntMap.
--  Data shown is 'PointSupply', number of points and number of sets.
instance Show PointEnvironment where
  show pe = "("++show (pointSupply pe)++" with "
    ++show (nPoints pe)++" points in "
    ++show (n_sets pe)++" sets)"

-- |Invokes the generator functions in the Common module to create a random
--  'CallList' and generates a 'PointEnvironment' from it.
instance Arbitrary PointEnvironment where
  arbitrary :: Gen PointEnvironment
  arbitrary = do
    cl <- arbitrary
    return $ runCalls cl
  shrink :: PointEnvironment -> [PointEnvironment]
  shrink (PE _ _ _ cl) = map runCalls (shrink cl)

-- |Generates a 'PointEnvironment' from a given 'CallList'. Internal use.
runCalls :: CallList -> PointEnvironment
runCalls cl@(CL cs) = foldl doCall (PE newPointSupply Map.empty 0 cl) cs

-- |Executes a 'Call' to a given 'PointEnvironment' and returns the result.
doCall :: PointEnvironment -> Call -> PointEnvironment
doCall pe@(PE pSup pM n cl) call = case call of
  Fresh _       -> let (pSup',p) = fresh pSup n
                   in  PE pSup' (Map.insert n p pM) (n+1) cl
  Union p1 p2   -> let p1' = fromJust $ Map.lookup p1 pM
                       p2' = fromJust $ Map.lookup p2 pM
                   in  pe{pointSupply = union pSup p1' p2'}

-- |Helper function for 'PointEnvironment': extracts nr. of sets.
n_sets :: PointEnvironment -> Int
n_sets pe = length $ List.nub $ map (descriptor pSup) (Map.elems pm)
  where
    pSup = pointSupply pe
    pm  = pointMap pe

-- |Helper function for 'PointEnvironment': extracts all representatives.
reps :: PointEnvironment -> [Point Int]
reps pe = snd $ unzip $ List.nubBy same descriptorRepr
  where
    descriptorRepr = [(descriptor pSup p,repr pSup p)|p<-Map.elems pm]
    same           = \(d,_) (d',_) -> d==d'
    pSup           = pointSupply pe
    pm             = pointMap pe

-- |Helper function for 'PointEnvironment': extracts all entries (Int,Point Int)
--  in the same set as the given point.
pairsOfSet :: PointEnvironment -> Point Int -> [(Int,Point Int)]
pairsOfSet pe r = [(k,p)|(k,p)<-Map.toList pm, p `sameSet` r]
  where
    pSup         = pointSupply pe
    pm          = pointMap pe
    sameSet a b = descriptor pSup a == descriptor pSup b

-- |Helper function for 'PointEnvironment': extracts all points in the same
--  set as the given point.
elemsOfSet :: PointEnvironment -> Point Int -> [Point Int]
elemsOfSet pe r = map snd $ pairsOfSet pe r

-- |Helper function for 'PointEnvironment': extracts all points, grouped up
--  by their respective sets.
pointsBySet :: PointEnvironment -> [[Point Int]]
pointsBySet pe = map (elemsOfSet pe) (reps pe)

-- |Helper function for 'PointEnvironment': decides whether two sets are equivalent,
--  i.e. if they have the same number of points, the same number of sets, each
--  with the same number of elements, and whether the points in each set have
--  the same key in the given PointEnvironment's inner map.
sameSets :: PointEnvironment -> PointEnvironment -> Bool
sameSets pe1 pe2 = sameNrPoints && sameNrSets && sameNrElems && samePoints
  where
    pm1          = pointMap pe1
    pm2          = pointMap pe2
    sets1        = pointsBySet pe1
    sets2        = pointsBySet pe2
    keysBySet pe = map (map fst) (map (pairsOfSet pe) (reps pe))
    sameNrPoints = length pm1 == length pm2
    sameNrSets   = length sets1 == length sets2
    sameNrElems  = all (\(x,y) -> length x == length y) (zip sets1 sets2)
    samePoints   = List.sort (keysBySet pe1) == List.sort (keysBySet pe2)

-- |Verifies property: given any environment, ensures that any fresh point
--  is always disjoint to all other points in the pointSupply.
prop_newPointDisjoint :: PointEnvironment -> Bool
prop_newPointDisjoint pe =
  let pSup      = pointSupply pe
      pm       = pointMap pe
      next     = nPoints pe
      (pSup',p) = fresh pSup next
  in  not $ or $ map (equivalent pSup' p) (Map.elems pm)

-- |Verifies property: given an environment with at least one point, ensures
--  that relationships between existing points are not affected by 'fresh'.
prop_sameReprAfterFresh :: PointEnvironment -> Bool
prop_sameReprAfterFresh pe = and $ map sameReprAsBefore points
  where
    pe'                = pe `doCall` (Fresh (nPoints pe))
    pSup               = pointSupply pe
    pSup'              = pointSupply pe'
    points             = Map.elems $ pointMap pe
    sameReprAsBefore p = descriptor pSup p == descriptor pSup' p

-- |Verifies property: given an environment with at least one point, ensures
--  that all points in a set have the same representative as the representative
--  of the set.
prop_sameReprAsRepr :: PointEnvironment -> Property
prop_sameReprAsRepr pe = n_sets pe > 0 ==> -- n = 0: trivially true
  let r = head $ reps pe
      pSup = pointSupply pe
      sameAs p p' = descriptor pSup p == descriptor pSup p'
  in  and $ map (\p -> p `sameAs` r) (pe `elemsOfSet` r)

-- |Verifies property: given an environment with at least two points, ensures
--  that if two points are in the same set, then their representative is the
--  same and vice versa.
prop_sameSetSameRepr :: PointEnvironment -> Property
prop_sameSetSameRepr pe = nPoints pe > 1 ==>
  let pSup = pointSupply pe
      sameAs p p' = descriptor pSup p == descriptor pSup p'
      p:points = elemsOfSet pe $ head $ reps pe
  in  and $ map (sameAs p) points

-- |Verifies property: given 'pSup' and two points 'p1', 'p2' contained in it,
--  after the execution of 'union' 'pSup' 'p1' 'p2' the two points will have
--  the same representative.
prop_sameReprAfterUnion :: PointEnvironment -> Property
prop_sameReprAfterUnion pe = n_sets pe > 1 ==>
  let ((p1:_):(p2:_):_) = pointsBySet pe
      pSup = pointSupply pe
      pSup' = union pSup p1 p2
      sameAs p p' = descriptor pSup p == descriptor pSup p'
  in  (repr pSup' p1) `sameAs` (repr pSup' p2)

-- |Verifies property: given 'pSup' and two points 'p1', 'p2' contained in it,
--  after the execution of 'pSup2' = 'union' 'pSup' 'p1' 'p2', invoking
--  'equivalent' 'pSup2' 'p1' 'p2' will return 'True'.
prop_sameSetAfterUnion :: PointEnvironment -> Property
prop_sameSetAfterUnion pe = n_sets pe > 1 ==>
  let ((p1:_):(p2:_):_) = pointsBySet pe
      pSup = pointSupply pe
      pSup' = union pSup p1 p2
  in  equivalent pSup' p1 p2

-- | Transitivity in respect to set membership equivalence.
-- x=y && y=z => x=z
prop_transitivity :: PointEnvironment -> Property
prop_transitivity (PE pSup pM n _cl) = n > 2 ==>
  and [not (equivalent pSup (pM!x) (pM!y) &&
            equivalent pSup (pM!y) (pM!z))
       || equivalent pSup (pM!x) (pM!z)
      | x<-[0..(n-1)], y<-[0..x], z<-[0..y]
      ]

-- | Commutativity in respect to set membership equivalence.
prop_commutativity_eq :: PointEnvironment -> Bool
prop_commutativity_eq (PE pSup pM n _cl) =
  and [equivalent pSup (pM!x) (pM!y) ==
       equivalent pSup (pM!y) (pM!x)
      | x<-[0..(n-1)], y<-[0..x]
      ]

-- | Reflexivity in respect to set membership equivalence.
prop_reflexivity_eq :: PointEnvironment -> Bool
prop_reflexivity_eq (PE pSup pM n _cl) =
  and $ [equivalent pSup (pM!x) (pM!x)
        | x<-[0..(n-1)]
        ]

-- | Associativity in respect to set membership equivalence.
prop_associativity :: PointEnvironment -> Property
prop_associativity pe@(PE pSup pM _n _cl) =
  let rs = reps pe
      left  = union (union pSup (rs!!0) (rs!!1)) (rs!!1) (rs!!2)
      right = union (union pSup (rs!!1) (rs!!2)) (rs!!0) (rs!!1)
  in  (length rs >= 3) ==>
        sameSets pe{pointSupply=left} pe{pointSupply=right}

