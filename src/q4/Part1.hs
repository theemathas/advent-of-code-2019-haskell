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
    consecutivePairs = zip digits (tail digits)
    hasConsecutiveSame = any (uncurry (==)) consecutivePairs
    nonDecreasing = all (uncurry (<=)) consecutivePairs

digitsOf :: Integer -> [Integer]
digitsOf number = assert (number > 0) (reverse (helper number))
  where
    helper 0 = []
    helper innerNumber = (innerNumber `mod` 10) : helper (innerNumber `div` 10)