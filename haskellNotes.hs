{-}
Tuples
fst (x, y) -> x
snd (x, y) --> y

Lists
head [x, y] -> x
tail [x, y] -> [y]

use [(x,y), (z,w)] for list of tuples structure

add stuff to lists with x:y:z:[] -> [x,y,z]

equality
> == equal
> /= not equal

three primary list-processing functions:
- map (only operates on lists, like Elixir)
    map (func) (list)
    map Data.Char.toUpper "Hello, World!"
    > "HELLO, WORLD!"
- filter
    filter (func) (list)
    filter Data.Char.isLower "Hello, World!"
    > "elloorld"
- foldr (and foldl)
    foldr is right associative; foldl is left associative
    replaces the cons (:) operator in lists to apply a function between ALL elements
    foldr (+) 0 [1, 2, 3, 4, 5] ...weird that you need a zero there or it breaks
    > foldr (+) 0 1:2:3:4:5:[]
    > 1+2+3+4+5+0
    > 15
    USE THIS to foldr each tuple into a string or tuple.

take 3 [1,2,3,4,5,6]
> [1,2,3] ...creates a subset list

drop 2 (take 3 [1,2,3,4,5,6])
> [3] ... it drops the first two elements from list; useful instead of doing hd(tl(tl(list)))

zip [9,10,11] [1,2,0]
> [(9,1), (10,2), (11,0)]

make your own functions
> square x = x*x
> sum a b c d = a + b + c + d

-}

-- cond

sign x
  | x < 0 = -1
  | x > 0 = 1
  | otherwise = 0

-- case
isNum x =
  case x of
    0 -> 0
    1 -> 1
    2 -> 2
    _ -> -1

colour rgb =
  case rgb of
    (255, _, _) -> "RED"
    (_, 255, _) -> "GREEN"
    (_, _, 255) -> "BLUE"
    (_, _, _) -> "None"

-- two expressions exist in this function now
example x = do
  let q = x
  if q < 0
    then -1
    else
      if q > 0
        then 1
        else 0