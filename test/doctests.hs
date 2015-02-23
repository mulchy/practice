module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/HW1/CreditCard.hs", "src/HW1/Hanoi.hs"]
