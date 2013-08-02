import System.Environment

indexList :: [a] -> [(Integer, a)]
indexList as = indexIt as 0
    where
        indexIt (a:as) i = (i,a):(indexIt as (i+1))
        indexIt [] _ = []

binaryToDecimal :: String -> Integer
binaryToDecimal num = foldr (\a b -> (toValue a) + b) 0 toMultipy
    where
        toValue (idx, '1') = 2^idx
        toValue (idx, '0') = 0
        toValue (_, _) = error "No binary string."
        toMultipy = indexList $ reverse num

log2 x = (log x) / (log 2)

decimalToBinary value = let toFlip = decimalToBinary' value in 
        flipIt toFlip (head toFlip)
    where
        flipIt _ (-1) = []
        flipIt [] idx = '0':flipIt [] (idx-1)
        flipIt (f:fs) idx = if f == idx then '1':(flipIt fs (idx-1)) else '0':(flipIt (f:fs) (idx-1))

decimalToBinary' :: Integer -> [Integer]
decimalToBinary' 0  = []
decimalToBinary' cur = 
    let lcur = fromIntegral $ floor $ log2 (fromIntegral cur) in
        lcur:(decimalToBinary' (cur-(2^lcur)))


dispatch args
    | (length args) /= 2 = putStrLn "usage: BinaryToDecimal <tobin/todec> number" 
    | (head args) == "todec" = putStrLn $ show $ binaryToDecimal $ (args !! 1)
    | (head args) == "tobin" = putStrLn $ decimalToBinary (read (args !! 1)::Integer)
    | otherwise = error "usage: BinaryToDecimal <tobin/todec> number"

main :: IO ()
main = do
    args <- getArgs
    dispatch args