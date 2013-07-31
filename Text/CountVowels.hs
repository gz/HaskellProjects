import Data.List
import System.Environment

vowels :: String -> [(Char, Int)]
vowels txt = [ (a, a `howManyIn` txt) | a <- "aeiou" ]
    where
        howManyIn _ [] = 0
        howManyIn a b = length $ findIndices (== a) b

main :: IO ()
main = do
    args <- getArgs
    txt <- readFile $ head args
    putStrLn $ show $ vowels txt