import System.Environment
import Data.List
import Test.HUnit

{-- API to Implement --}

-- | Returns the fibonacci sequence as a list.
-- The sequence length is specified by the first argument.
fibonacciNumbers :: Int -> [Int]

-- | Prints the fibonacci sequence as a list on standard out.
-- Takes as a command line argument the length of the sequence.
main :: IO ()


{-- Tests --}

testNumbers1 = TestCase $ assertEqual "fibonacciNumbers 12" [0,1,1,2,3,5,8,13,21,34,55,89] (fibonacciNumbers 12)
testNumbers2 = TestCase $ assertEqual "fibonacciNumbers 1" [0] (fibonacciNumbers 1)
testNumbers3 = TestCase $ assertEqual "fibonacciNumbers 0" [] (fibonacciNumbers 0)
testNumbers4 = TestCase $ assertEqual "fibonacciNumbers -1" [] (fibonacciNumbers (-1))

tests = TestList [testNumbers1, testNumbers2, testNumbers3]


{-- Implementation --}

fibonacciSequence :: Int -> Int
fibonacciSequence 0 = 0
fibonacciSequence 1 = 1
fibonacciSequence n = fibonacciSequence (n-1) + fibonacciSequence (n-2)

fibonacciNumbers len = [ fibonacciSequence n | n <- [0..len-1] ]

main = do
    args <- getArgs
    putStrLn $ show (fibonacciNumbers (read (head args)::Int))
