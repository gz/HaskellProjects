import NextPrimeNumber
import System.Environment


{-- API to Implement --}

-- | Returns a list of prime factors for a given number.
primeFactors :: Int -> [Int]

-- | Takes a number as an argument, prints the list of prime factors for the number.
main :: IO ()


{-- Implementation --}

primeFactors n = [ f | f <- (takeWhile (< n) NextPrimeNumber.primes), n `mod` f == 0 ]

main = do
    args <- getArgs
    putStrLn $ show $ primeFactors (read (head args)::Int)