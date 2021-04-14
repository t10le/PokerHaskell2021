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

-- HIGH RANK PATTERN TESTS --
testHighRank =
  TestLabel "HighRank: --> Hand 1" $
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
  TestLabel "HighRank: --> Hand 2" $
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
  TestLabel "HighRank: --> KICKER --> Hand 1" $
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
  TestLabel "HighRank: --> KICKER --> Hand 2" $
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
        testHighRank,
        testHighRank2,
        testHighRank4,
        testHighRank4
      ]