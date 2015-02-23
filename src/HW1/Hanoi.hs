-- |
-- Tower of Hanoi
module HW1.Hanoi where

-- |
-- The name of a peg
type Peg = String

-- |
-- Represents a move from peg fst to snd
type Move = (Peg, Peg)

-- |
-- Generates a list of moves needed to move n discs from the first peg
-- to the second peg where a, b, and c are the names of the pegs
--
-- >>> hanoi 0 "a" "b" "c"
-- []
--
-- >>> hanoi 1 "a" "b" "c"
-- [("a","b")]
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)
