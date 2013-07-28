import System.IO

split :: Char -> String -> String -> [String] -> [String]
split _ [] cur wlist = cur:wlist
split s (x:text) cur wlist
    | s == x = split s text "" (cur:wlist)
    | otherwise = split s text (x:cur) wlist

--wordCount :: String ->  Int
wordCount x = length $ split ' ' x "" []

main :: IO ()
main = do
    handle <- openFile "README.md" ReadMode
    contents <- hGetContents handle
    putStrLn "The wordcount for README.md is: "
    putStrLn $ show $ wordCount contents
    hClose handle
