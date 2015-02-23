-- |
-- The first HW assignment.
module HW1.CreditCard (toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate) where
import Data.Char

-- |
-- Returns a list of digits.
--
-- >>> toDigits 1234
-- [1,2,3,4]
--
-- >>> toDigits 0
-- []
--
-- >>> toDigits (-17)
-- []
toDigits :: Integer -> [Integer]

-- |
-- Returns a reversed list of digits.
--
-- >>> toDigitsRev 1234
-- [4,3,2,1]
toDigitsRev :: Integer -> [Integer]

-- |
-- Doubles every other element, starting from the right
--
-- >>> doubleEveryOther [8,7,6,5]
-- [16,7,12,5]
--
-- >>> doubleEveryOther [1,2,3]
-- [1,4,3]
doubleEveryOther :: [Integer] -> [Integer]

-- |
-- Returns the sum of the sum of the digits of the elements of the list.
--
-- >>> sumDigits [12,3,4]
-- 10
--
-- >>> sumDigits [16,7,12,5]
-- 22
sumDigits :: [Integer] -> Integer

-- |
-- Validates a "credit card number". Doubles every other element of the list, starting from the right,
-- calculates the sum of all of the digits, and returns true if the result is divisible by 10.
--
-- >>> validate 4012888888881881
-- True
--
-- >>> validate 4012888888881882
-- False
validate :: Integer -> Bool

toDigits n
 | n <= 0 = []
 | otherwise = map fromIntegral . map digitToInt . show $ n

toDigitsRev = reverse . toDigits

doubleEveryOther x = reverse (dropSecond ( map doubleSecond (tagItems (reverse x))))

sumDigits = sum . concat . map toDigits

validate = validateChecksum . checksum

--private functions
tagItems :: (Num b) => [a] -> [(a,b)]
tagItems x = zip x (take (length x) $ cycle [1,2])

doubleSecond :: (Num a, Num b, Eq b) => (a,b) -> (a,b)
doubleSecond (a, b)
  | b == 2 = (2 * a, b)
  | otherwise = (a,b)

dropSecond :: [(a,b)] -> [a]
dropSecond = map fst

type Checksum = Integer

checksum :: Integer -> Checksum
checksum x = fromInteger (sumDigits $ doubleEveryOther $ toDigits x) `rem` 10

validateChecksum :: Checksum -> Bool
validateChecksum x
  | x == 0 = True
  | otherwise = False
