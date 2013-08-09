import System.Environment
import Numeric

doTerm :: Integer -> Double
doTerm k = (fromIntegral $ (2^k)*(product [1..k])^2) / (fromIntegral $ product [1..(2*k)+1])

halfPi :: Integer -> Double
halfPi n = sum [ doTerm k | k <- [0..n] ]

getPi :: Integer -> Double
getPi n = 2 * halfPi n

digitLimit :: Integer
digitLimit = 10

digitsToIterations 1  = 5
digitsToIterations 2  = 9
digitsToIterations 3  = 10
digitsToIterations 4  = 13
digitsToIterations 5  = 18
digitsToIterations 6  = 20
digitsToIterations 7  = 23
digitsToIterations 8  = 27
digitsToIterations 9  = 29
digitsToIterations 10 = 32
digitsToIterations _ = error "Needs improvement before we can go further :-("

-- 3.1415926535
main :: IO ()
main = do
    args <- getArgs
    let digits = read (head args)::Integer
    let digitsInt = read (head args)::Int
    putStrLn $ "Pi is ≅ " ++ showGFloat (Just digitsInt) (getPi $ digitsToIterations digits) ""
    return ()