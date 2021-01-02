import Control.Exception (assert)
import Relude hiding (tail)
import Relude.Unsafe (tail)

inputFrom :: Integer
inputFrom = 387638

inputTo :: Integer
inputTo = 919123

main :: IO ()
main = do
  let validPasswords = filter isValid [inputFrom .. inputTo]
  print $ length validPasswords

isValid :: Integer -> Bool
isValid password = length digits == 6 && hasConsecutiveSame && nonDecreasing
  where
    digits = digitsOf password

    [d0, d1, d2, d3, d4, d5] = digits
    hasConsecutiveSame =
      (d0 == d1 && d1 /= d2)
        || (d0 /= d1 && d1 == d2 && d2 /= d3)
        || (d1 /= d2 && d2 == d3 && d3 /= d4)
        || (d2 /= d3 && d3 == d4 && d4 /= d5)
        || (d3 /= d4 && d4 == d5)

    -- ((digits !! 0 == digits !! 1) && (digits !! 1 /= digits !! 2))
    --   || ((digits !! 0 /= digits !! 1) && (digits !! 1 == digits !! 2) && (digits !! 2 /= digits !! 3))
    --   || ((digits !! 1 /= digits !! 2) && (digits !! 2 == digits !! 3) && (digits !! 3 /= digits !! 4))
    --   || ((digits !! 2 /= digits !! 3) && (digits !! 3 == digits !! 4) && (digits !! 4 /= digits !! 5))
    --   || ((digits !! 3 /= digits !! 4) && (digits !! 4 == digits !! 5))

    consecutivePairs = zip digits (tail digits)
    nonDecreasing = all (uncurry (<=)) consecutivePairs

digitsOf :: Integer -> [Integer]
digitsOf number = assert (number > 0) (reverse (helper number))
  where
    helper 0 = []
    helper innerNumber = (innerNumber `mod` 10) : helper (innerNumber `div` 10)