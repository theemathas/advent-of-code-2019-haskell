module Main where

main :: IO ()
main = do
    putStrLn "something"
    print $ head [1, 2, 3]
    print $ head ([] :: [Int])
