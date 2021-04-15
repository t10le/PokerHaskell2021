-- Tommy Le (500793841)
module Poker where

import Data.List

-- | A helper function to initialize a tuple for recursive function, reduce'.
reduce x = reduce' (x, 0)

-- | Returns the tuple of a card in the form of (rank, suit).
-- The parameter, 'x', is an integer representing the card between (1..52).
-- The parameter, 'n', is an integer representing the suit between (0, 3).
--
-- Prelude> reduce 42
-- (3, 3)
reduce' (x, n)
  | x `elem` [1 .. 13] = (x, n)
  | otherwise = reduce' (x - 13, n + 1)

-- | Returns the string form of a card translated as "rankSuit".
-- The parameter, 'x', is an integer representing the card between (1..52).
-- The parameter, 'n', is an integer representing the suit between (0, 3).
--
-- Prelude> interpretCard (13, 0)
-- "13C"
interpretCard (x, n) =
  show x ++ [suit !! n]
  where
    suit = ['C', 'D', 'H', 'S']

-- | Returns the winning tuple (rank, suit) during a tie or kicker scenario between two hands.
kicker x y = kicker' (reverse (sort x)) (reverse (sort y)) 0

kicker' x y z
  | (fst (last x) == 1) && (fst (last y) /= 1) = last x
  | (fst (last y) == 1) && (fst (last x) /= 1) = last y
  | x !! z == y !! z = kicker' x y (z + 1)
  | x !! z > y !! z = x !! z
  | x !! z < y !! z = y !! z

-- | Returns the winning list of tuples [(rank, suit)...(rank,suit)] during a tie or kicker scenario between two hands.
kickerStraight x y = kickerStraight' x y 0

kickerStraight' x y z
  | x !! z == y !! z = kickerStraight' x y (z + 1)
  | x !! z > y !! z = x
  | x !! z < y !! z = y

-- | Returns the occurrence of a target value from an array.
countRecursion [] find = 0
countRecursion (x : xs) find
  | find == x = 1 + countRecursion xs find
  | otherwise = countRecursion xs find

-- | Returns an integer representing the most common suit (count >= 5) that matches royal flush pattern.
-- A helper function for ifRoyalFlush.
--
-- Prelude> findCommonSuit [(1,3),(3,3),(9,3),(10,3),(11,3),(12,3),(13,3)]
-- 3
findCommonSuit hand
  | null bestSuit = -1
  | snd (head bestSuit) >= 5 = fst (head bestSuit)
  | otherwise = -1
  where
    arr = map snd hand
    test x = snd x >= 5
    bestSuit = filter test (zip [0, 1, 2, 3] [countRecursion arr x | x <- [0 .. 3]])

-- | Returns a boolean value on whether the target hand + pool has the target suit pattern for royal flush.
-- A helper function for ifRoyalFlush.
--
-- Prelude> containsRoyalFlush 3 [(1,3),(3,3),(9,3),(10,3),(11,3),(12,3),(13,3)]
-- True
containsRoyalFlush suit arr = ((10, suit) `elem` arr) && ((11, suit) `elem` arr) && ((12, suit) `elem` arr) && ((13, suit) `elem` arr) && ((1, suit) `elem` arr)

-- | Returns a list of tuples representing the royal flush sequence if found for a player; otherwise returns an empty array.
--
-- Prelude> ifRoyalFlush [(1,3),(3,3),(9,3),(10,3),(11,3),(12,3),(13,3)] [(2,3),(4,3),(9,3),(10,3),(11,3),(12,3),(13,3)]
-- ["10S", "11S", "12S", "13S", "1S"]
ifRoyalFlush hand1 hand2
  | containsRoyalFlush player1Suit hand1 = map interpretCard [(10, player1Suit), (11, player1Suit), (12, player1Suit), (13, player1Suit), (1, player1Suit)]
  | containsRoyalFlush player2Suit hand2 = map interpretCard [(10, player2Suit), (11, player2Suit), (12, player2Suit), (13, player2Suit), (1, player2Suit)]
  | otherwise = []
  where
    player1Suit = findCommonSuit hand1
    player2Suit = findCommonSuit hand2

-- | Returns a list of only sequential (+1) ranks [1,2,3,4,5] corresponding to the straight pattern.
-- A helper function for ifStraightFlush.
containsStraight hand
  | case1 && (fst (head hand) `elem` arr || fst (hand !! 1) `elem` arr) = arr
  | case2 && (fst (head hand) `elem` brr || fst (hand !! 1) `elem` brr) = brr
  | case3 && (fst (head hand) `elem` crr || fst (hand !! 1) `elem` crr) = crr
  | otherwise = []
  where
    rank = map fst (reverse (sort hand))
    case1 = (head rank `elem` rank) && (head rank -1 `elem` rank) && (head rank -2 `elem` rank) && (head rank -3 `elem` rank) && (head rank -4 `elem` rank)
    case2 = ((rank !! 1) `elem` rank) && ((rank !! 1) -1 `elem` rank) && ((rank !! 1) -2 `elem` rank) && ((rank !! 1) -3 `elem` rank) && ((rank !! 1) -4 `elem` rank)
    case3 = ((rank !! 2) `elem` rank) && ((rank !! 2) -1 `elem` rank) && ((rank !! 2) -2 `elem` rank) && ((rank !! 2) -3 `elem` rank) && ((rank !! 2) -4 `elem` rank)
    arr = [head rank, head rank -1, head rank -2, head rank -3, head rank -4]
    brr = [rank !! 1, (rank !! 1) -1, (rank !! 1) -2, (rank !! 1) -3, (rank !! 1) -4]
    crr = [rank !! 2, (rank !! 2) -1, (rank !! 2) -2, (rank !! 2) -3, (rank !! 2) -4]

-- | Returns a list of tuples representing the straight flush sequence if found for a player; kicker function is implied; otherwise returns an empty array.
ifStraightFlush hand1 hand2
  | (hand1 `intersect` player1Cards /= []) && (hand2 `intersect` player2Cards /= []) = map interpretCard (reverse (kickerStraight player1Cards player2Cards))
  | hand1 `intersect` player1Cards /= [] = map interpretCard (reverse player1Cards)
  | hand2 `intersect` player2Cards /= [] = map interpretCard (reverse player2Cards)
  | otherwise = []
  where
    player1Suit = findCommonSuit hand1
    player2Suit = findCommonSuit hand2
    player1Cards = zip (containsStraight hand1) (replicate 5 player1Suit)
    player2Cards = zip (containsStraight hand2) (replicate 5 player2Suit)

-- | Returns the list of tuples representing the flush sequence.
-- A helper function for ifFlush.
containsFlush hand
  | length arr < 5 = []
  | (head hand `elem` arr || hand !! 1 `elem` arr) && length arr == 5 = arr
  | head hand /= head arr && hand !! 1 /= head arr && length (tail arr) == 5 = tail arr
  | otherwise = []
  where
    playerSuit = findCommonSuit hand
    test x = snd x == playerSuit
    arr = reverse (sort (filter test hand))

-- | Returns a list of tuples representing the flush sequence if found for a player; kicker function is implied; otherwise returns an empty array.
ifFlush hand1 hand2
  | containsFlush hand1 /= [] && containsFlush hand2 /= [] = map interpretCard (reverse (kickerStraight (containsFlush hand1) (containsFlush hand2)))
  | containsFlush hand1 /= [] = map interpretCard (reverse (containsFlush hand1))
  | containsFlush hand2 /= [] = map interpretCard (reverse (containsFlush hand2))
  | otherwise = []

-- | Returns a list of tuples representing the highest card for a player; kicker function is implied.
isHighCard hand1 hand2
  | maximum hand1 == maximum hand2 = [interpretCard (kicker hand1 hand2)]
  | maximum hand1 > maximum hand2 = [interpretCard (maximum hand1)]
  | maximum hand1 < maximum hand2 = [interpretCard (maximum hand2)]

-- | Returns a list containing the winning poker combination in string form.
-- Step 1: Initialize hand1 and hand2 relative to pool.
-- Step 2: Decode each hand's integer values into an array of tuples containing the rank, suit and string equivalent.
-- Step 3: Compare each hand's ranks and suits to pattern match a poker combination, starting from best combination to
--         the world, then interpret and print the winning hand.
deal x
  | ifRoyalFlush hand1 hand2 /= [] = ifRoyalFlush hand1 hand2
  | ifStraightFlush hand1 hand2 /= [] = ifStraightFlush hand1 hand2
  | ifFlush hand1 hand2 /= [] = ifFlush hand1 hand2
  | otherwise = isHighCard hand1 hand2
  where
    hand1 = map reduce ([head x] ++ [x !! 2] ++ drop 4 x)
    hand2 = map reduce ([x !! 1] ++ [x !! 3] ++ drop 4 x)

-- Temporary, DELETE before submission.
-- DO NOT USE 'a' OR 'b' as variables!!!!
a x = map reduce ([head x] ++ [x !! 2] ++ drop 4 x)

b x = map reduce ([x !! 1] ++ [x !! 3] ++ drop 4 x)

{-

FLUSH
x = [27, 45, 3, 48, 44, 43, 41, 33, 12]

TESTING
ghci -w simple_tester_haskell.hs
runTests

ghci -w haskell_poker_tester.hs
test

-}