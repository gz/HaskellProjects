import System.Environment   
import Data.List  

coins :: [Int]
coins = [1, 5, 10, 20, 50, 100, 500]

howManyCoins :: Int -> [(Int, Int)]
howManyCoins amount = howManyCoins' amount [] 0 (reverse coins)

howManyCoins' 0 change index (coin:coins) = (coin, index):change
howManyCoins' amountLeft change _ [] = change
howManyCoins' amountLeft change index (coin:coins)
    | amountLeft >= coin = howManyCoins' (amountLeft-coin) change (index+1) (coin:coins)
    | index > 0 && amountLeft < coin = howManyCoins' amountLeft ((coin, index):change) 0 coins
    | index == 0 && amountLeft < coin = howManyCoins' amountLeft change 0 coins

returnAmount :: Int -> Int -> [(Int, Int)]
returnAmount cost payment = howManyCoins change 
    where change = (payment - cost)

main = do  
   args <- getArgs  
   let cost = read (args !! 0)::Int in
    let payed = read (args !! 1)::Int in
        putStrLn $ show (returnAmount cost payed)