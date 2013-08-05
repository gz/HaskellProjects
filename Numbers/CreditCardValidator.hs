import System.Environment
import Data.Char

lastDigit = last . map digitToInt . show

computeCheckDigit = lastDigit . (*9) . sum . sumIfGreater9 . map doubleEveryOther . zip [1..] . toDigits 
    where
        sumIfGreater9 = map (\x -> if x > 9 then digsum x else x) 
        doubleEveryOther (idx, n)
            | idx `mod` 2 == 1 = n*2
            | otherwise = n
        toDigits = map digitToInt
        digsum 0 = 0
        digsum x = (x `mod` 10) + digsum (x `div` 10)

main = do
    args <- getArgs
    if digitToInt (last (head args)) == computeCheckDigit ((tail . reverse . head) args) then
        putStrLn "Last digit is valid checksum."
    else
        putStrLn "Is digit is invalid checksum."