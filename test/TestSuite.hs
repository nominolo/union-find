module TestSuite (tests) where

import           Distribution.TestSuite
import           Distribution.TestSuite.QuickCheck
import qualified Common
import qualified IntMapProps

tests :: IO [Test]
tests = return
  [ Group "Common (test model)" True [
      testProperty "refsInRange"        Common.prop_refsInRange
    , testProperty "refsInRange_shrink" Common.prop_refsInRange_shrink
    , testProperty "pointsContinious"   Common.prop_pointsContinious
    , testProperty "pointsContinious_shrink" Common.prop_pointsContinious_shrink
    ],
    Group "IntMap" True [
      testProperty "newPointDisjoint"   IntMapProps.prop_newPointDisjoint
    , testProperty "sameReprAfterFresh" IntMapProps.prop_sameReprAfterFresh
    , testProperty "sameReprAsRepr"     IntMapProps.prop_sameReprAsRepr
    , testProperty "sameSetSameRepr"    IntMapProps.prop_sameSetSameRepr
    , testProperty "sameReprAfterUnion" IntMapProps.prop_sameReprAfterUnion
    , testProperty "sameSetAfterUnion"  IntMapProps.prop_sameSetAfterUnion
    , testProperty "transitivity"       IntMapProps.prop_transitivity
    , testProperty "commutativity_eq"   IntMapProps.prop_commutativity_eq
    , testProperty "reflexivity_eq"     IntMapProps.prop_reflexivity_eq
    , testProperty "associativity"      IntMapProps.prop_associativity
    ]
  ]


