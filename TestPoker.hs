module TestPoker where

import Poker
import Test.HUnit

-- ROYAL FLUSH PATTERN TESTS --
testRoyalFlush =
  TestLabel "RoyalFlush --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "RoyalFlush: Player 1 should win."
            ["10S", "11S", "12S", "13S", "1S"]
            (deal [40, 41, 42, 43, 48, 49, 50, 51, 52]),
          assertEqual
            "RoyalFlush: Player 1 should win."
            ["10H", "11H", "12H", "13H", "1H"]
            (deal [27, 13, 38, 26, 34, 47, 36, 37, 39])
        ]

testRoyalFlush2 =
  TestLabel "RoyalFlush --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "RoyalFlush: Player 2 should win."
            ["10S", "11S", "12S", "13S", "1S"]
            (deal [41, 40, 43, 42, 48, 49, 50, 51, 52]),
          assertEqual
            "RoyalFlush: Player 2 should win."
            ["10H", "11H", "12H", "13H", "1H"]
            (deal [13, 27, 26, 38, 34, 47, 36, 37, 39])
        ]

-- STRAIGHT FLUSH PATTERN TESTS --
testStraightFlush =
  TestLabel "StraightFlush --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "StraightFlush: Player 1 should win."
            ["6H", "7H", "8H", "9H", "10H"]
            (deal [32, 17, 33, 18, 19, 20, 34, 35, 36]),
          assertEqual
            "StraightFlush: Player 1 should win."
            ["7H", "8H", "9H", "10H", "11H"]
            (deal [32, 17, 33, 18, 34, 35, 36, 37, 38]),
          assertEqual
            "StraightFlush: Player 1 should win."
            ["4S", "5S", "6S", "7S", "8S"]
            (deal [42, 27, 43, 28, 45, 44, 46, 48, 47]),
          assertEqual
            "StraightFlush: Player 1 should win."
            ["6S", "7S", "8S", "9S", "10S"]
            (deal [43, 27, 49, 28, 45, 44, 46, 47, 48])
        ]

testStraightFlush2 =
  TestLabel "StraightFlush --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "StraightFlush: Player 2 should win."
            ["6H", "7H", "8H", "9H", "10H"]
            (deal [17, 32, 18, 33, 19, 20, 34, 35, 36]),
          assertEqual
            "StraightFlush: Player 2 should win."
            ["7H", "8H", "9H", "10H", "11H"]
            (deal [17, 32, 18, 33, 34, 35, 36, 37, 38]),
          assertEqual
            "StraightFlush: Player 2 should win."
            ["4S", "5S", "6S", "7S", "8S"]
            (deal [27, 42, 28, 43, 45, 44, 46, 48, 47]),
          assertEqual
            "StraightFlush: Player 2 should win."
            ["6S", "7S", "8S", "9S", "10S"]
            (deal [27, 43, 28, 49, 45, 44, 46, 47, 48])
        ]

testStraightFlush3 =
  TestLabel "StraightFlush --> KICKER --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "StraightFlush: (Kicker) Player 1 should win."
            ["2C", "3C", "4C", "5C", "6C"]
            (deal [8, 9, 6, 7, 5, 4, 3, 2, 1]),
          assertEqual
            "StraightFlush: (Kicker) Player 1 should win."
            ["9H", "10H", "11H", "12H", "13H"]
            (deal [32, 31, 39, 33, 34, 35, 36, 37, 38]),
          assertEqual
            "StraightFlush: (Kicker) Player 1 should win."
            ["8H", "9H", "10H", "11H", "12H"]
            (deal [31, 30, 38, 32, 33, 34, 35, 36, 37]),
          assertEqual
            "StraightFlush: (Kicker) Player 1 should win."
            ["7S", "8S", "9S", "10S", "11S"]
            (deal [47, 42, 50, 43, 44, 45, 46, 48, 49]),
          assertEqual
            "StraightFlush: (Kicker) Player 1 should win."
            ["5D", "6D", "7D", "8D", "9D"]
            (deal [19, 14, 22, 15, 16, 17, 20, 21, 18]),
          assertEqual
            "StraightFlush: (Kicker) Player 1 should win."
            ["6S", "7S", "8S", "9S", "10S"]
            (deal [31, 27, 49, 43, 44, 45, 46, 47, 48])
        ]

testStraightFlush4 =
  TestLabel "StraightFlush --> KICKER --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "StraightFlush: (Kicker) Player 2 should win."
            ["2C", "3C", "4C", "5C", "6C"]
            (deal [9, 8, 7, 6, 5, 4, 3, 2, 1]),
          assertEqual
            "StraightFlush: (Kicker) Player 2 should win."
            ["9H", "10H", "11H", "12H", "13H"]
            (deal [31, 32, 33, 39, 34, 35, 36, 37, 38]),
          assertEqual
            "StraightFlush: (Kicker) Player 2 should win."
            ["8H", "9H", "10H", "11H", "12H"]
            (deal [30, 31, 32, 38, 33, 34, 35, 36, 37]),
          assertEqual
            "StraightFlush: (Kicker) Player 2 should win."
            ["7S", "8S", "9S", "10S", "11S"]
            (deal [42, 47, 43, 50, 44, 45, 46, 48, 49]),
          assertEqual
            "StraightFlush: (Kicker) Player 2 should win."
            ["5D", "6D", "7D", "8D", "9D"]
            (deal [14, 19, 15, 22, 16, 17, 20, 21, 18]),
          assertEqual
            "StraightFlush: (Kicker) Player 2 should win."
            ["6S", "7S", "8S", "9S", "10S"]
            (deal [27, 31, 43, 49, 44, 45, 46, 47, 48])
        ]

-- FLUSH PATTERN TESTS --
testFlush =
  TestLabel "Flush --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "Flush: Player 1 should win."
            ["2S", "4S", "5S", "6S", "9S"]
            (deal [27, 45, 3, 48, 44, 43, 41, 33, 12]),
          assertEqual
            "Flush: Player 1 should win."
            ["1C", "4C", "6C", "7C", "10C"]
            (deal [1, 16, 14, 17, 4, 6, 7, 10, 11]),
          assertEqual
            "Flush: Player 1 should win."
            ["3D", "6D", "8D", "9D", "12D"]
            (deal [16, 31, 29, 32, 19, 21, 22, 34, 25]),
          assertEqual
            "Flush: Player 1 should win."
            ["3D", "6D", "8D", "9D", "13D"]
            (deal [16, 31, 26, 32, 19, 21, 22, 34, 47])
        ]

testFlush2 =
  TestLabel "Flush --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "Flush: Player 2 should win."
            ["2S", "4S", "5S", "6S", "9S"]
            (deal [45, 27, 48, 3, 44, 43, 41, 33, 12]),
          assertEqual
            "Flush: Player 2 should win."
            ["1C", "4C", "6C", "7C", "10C"]
            (deal [16, 1, 17, 14, 4, 6, 7, 10, 11]),
          assertEqual
            "Flush: Player 2 should win."
            ["3D", "6D", "8D", "9D", "12D"]
            (deal [31, 16, 32, 29, 19, 21, 22, 34, 25]),
          assertEqual
            "Flush: Player 2 should win."
            ["3D", "6D", "8D", "9D", "13D"]
            (deal [31, 16, 32, 26, 19, 21, 22, 34, 47])
        ]

testFlush3 =
  TestLabel "Flush --> KICKER --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "Flush: (Kicker) Player 1 should win."
            ["3D", "6D", "8D", "9D", "13D"]
            (deal [16, 15, 26, 20, 19, 21, 22, 34, 47])
        ]

testFlush4 =
  TestLabel "Flush --> KICKER --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "Flush: (Kicker) Player 2 should win."
            ["3D", "6D", "8D", "9D", "13D"]
            (deal [15, 16, 20, 26, 19, 21, 22, 34, 47])
        ]

-- PAIR PATTERN TESTS--
testPair =
  TestLabel "Pair --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "Pair: Player 1 should win."
            ["3C", "3D"]
            (deal [3, 1, 16, 2, 7, 8, 20, 33, 46])
        ]

testPair2 =
  TestLabel "Pair --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "Pair: Player 2 should win."
            ["3C", "3D"]
            (deal [1, 3, 2, 16, 7, 8, 20, 33, 46])
        ]

testPair3 =
  TestLabel "Pair --> KICKER --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "Pair: (Kicker) Player 1 should win."
            ["1H", "1S"]
            (deal [40, 52, 46, 11, 48, 27, 29, 32, 37]),
          assertEqual
            "Pair: (Kicker) Player 1 should win."
            ["13C", "13S"]
            (deal [13, 25, 8, 20, 40, 2, 16, 52, 51]),
          assertEqual
            "Pair: (Kicker) Player 1 should win."
            ["1C", "1D"]
            (deal [1, 2, 14, 15, 5, 18, 31, 44, 6])
        ]

testPair4 =
  TestLabel "Pair --> KICKER --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "Pair: (Kicker) Player 2 should win."
            ["1H", "1S"]
            (deal [52, 40, 11, 46, 48, 27, 29, 32, 37]),
          assertEqual
            "Pair: (Kicker) Player 2 should win."
            ["13C", "13S"]
            (deal [25, 13, 20, 8, 40, 2, 16, 52, 51]),
          assertEqual
            "Pair: (Kicker) Player 2 should win."
            ["1C", "1D"]
            (deal [2, 1, 15, 14, 5, 18, 31, 44, 6])
        ]

-- HIGH RANK PATTERN TESTS --
testHighRank =
  TestLabel "HighRank --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "HighRank: Player 1 should win."
            ["1C"]
            (deal [1, 3, 4, 2, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: Player 1 should win"
            ["5C"]
            (deal [2, 3, 5, 4, 8, 21, 34, 47, 9]),
          assertEqual
            "HighRank: Player 1 should win."
            ["1D"]
            (deal [14, 3, 4, 2, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: Player 1 should win."
            ["1H"]
            (deal [27, 3, 4, 2, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: Player 1 should win."
            ["1S"]
            (deal [40, 3, 4, 2, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: Player 1 should win."
            ["13H"]
            (deal [2, 3, 39, 4, 8, 21, 34, 47, 9])
        ]

testHighRank2 =
  TestLabel "HighRank --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "HighRank: Player 2 should win."
            ["1C"]
            (deal [3, 1, 2, 4, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: Player 2 should win"
            ["5C"]
            (deal [3, 2, 4, 5, 8, 21, 34, 47, 9]),
          assertEqual
            "HighRank: Player 2 should win."
            ["1D"]
            (deal [3, 14, 2, 4, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: Player 2 should win."
            ["1H"]
            (deal [3, 27, 2, 4, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: Player 2 should win."
            ["1S"]
            (deal [3, 40, 2, 4, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: Player 2 should win"
            ["13H"]
            (deal [3, 2, 4, 39, 8, 21, 34, 47, 9])
        ]

testHighRank3 =
  TestLabel "HighRank --> KICKER --> Hand 1" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "HighRank: (Kicker) Player 1 should win."
            ["4C"]
            (deal [1, 2, 4, 14, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: (Kicker) Player 1 should win."
            ["5C"]
            (deal [17, 3, 5, 4, 8, 9, 21, 34, 47])
        ]

testHighRank4 =
  TestLabel "HighRank --> KICKER --> Hand 2" $
    TestList $
      fmap
        TestCase
        [ assertEqual
            "HighRank: (Kicker) Player 2 should win."
            ["4C"]
            (deal [2, 1, 14, 4, 7, 20, 33, 46, 8]),
          assertEqual
            "HighRank: (Kicker) Player 2 should win."
            ["5C"]
            (deal [3, 17, 4, 5, 8, 9, 21, 34, 47])
        ]

main :: IO Counts
main =
  runTestTT $
    TestList
      [ testRoyalFlush,
        testRoyalFlush2,
        testStraightFlush,
        testStraightFlush2,
        testStraightFlush3,
        testStraightFlush4,
        testFlush,
        testFlush2,
        testFlush3,
        testFlush4,
        testPair,
        testPair2,
        testPair3,
        testPair4,
        testHighRank,
        testHighRank2,
        testHighRank4,
        testHighRank4
      ]