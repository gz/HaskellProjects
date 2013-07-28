import System.Environment
import System.Exit
import System.IO

import Data.List
import Test.HUnit

{-- API to Implement --}

-- | Returns the i-th Prime number.
ithPrime :: Int -> Int

-- | Prints prime numbers. Asks permission to continue 
-- before calculating each number.
main :: IO ()


{-- Tests --}
--testPrime1 = TestCase $ assertFailure "ithPrime -1" 0 (ithPrime (-1))
--testPrime2 = TestCase $ assertFailure "ithPrime 0" 1 (ithPrime 0)
testPrime3 = TestCase $ assertEqual "First prime is two." 2 (ithPrime 1)
testPrime4 = TestCase $ assertEqual "13th Prime is 41." 41 (ithPrime 13)

tests = TestList [testPrime3, testPrime4]


{-- Implementation --}
isPrim' x 1 = True
isPrim' x y = (not (x `mod` y == 0)) && isPrim' x (y-1)

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime x = isPrim' x (x-1)

primes = [ x | x <- [1..], isPrime x]
ithPrime idx = primes !! (idx-1)

askForPrime x = do
    putStr "Compute next Prime Number (y/n)? "
    hFlush stdout
    answer <- getLine
    if answer == "y"
        then do
            putStrLn $ show (ithPrime x)
            askForPrime (x+1)
        else
            if answer == "n" then do
                exitWith ExitSuccess
            else
                askForPrime x

main = do
    askForPrime 1