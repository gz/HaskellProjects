import Data.List
import System.Environment

vowels = "aeiou"

transformWord (c:cs)
    | c `elem` vowels = vowelWord (c:cs)
    | otherwise = consonantWord (c:cs)

vowelWord (c:cs) = cs ++ [c, 'a', 'y']

consonantWord ('y':cs) = cs ++ ['y', 'a', 'y']
consonantWord (c:cs) = cs ++ ['w', 'a', 'y']

main :: IO ()
main = do
    args <- getArgs
    txt <- readFile $ head args
    putStrLn $ unwords $ map transformWord (words txt)
