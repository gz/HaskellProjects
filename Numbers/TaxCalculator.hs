import System.Environment

interest :: Float -> Float -> Float
interest amount tax = amount * (tax / 100.0)

cost :: Float -> Float -> (Float, Float)
cost amount tax = (amount + (interest amount tax), (interest amount tax))

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Cost, Tax = " ++ (show $ cost (read (args !! 0)::Float) (read (args !! 1)::Float))