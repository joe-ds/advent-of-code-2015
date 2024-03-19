import System.IO

interpret :: Char -> Float
interpret '(' = 1
interpret ')' = -1
interpret _   = 0

partOne :: String -> Float 
partOne = sum . map interpret

looper :: String -> Integer -> Float -> Integer 
looper "" i _ = i
looper (c:cs) i f
                | f < 0 = i
                | otherwise =  looper cs (i + 1) (f + interpret c)

partTwo :: String -> Integer
partTwo cs = looper cs 0 0.0

main :: IO ()
main = do
    f <- openFile "input" ReadMode
    input <- hGetContents  f
    putStr "Part One completed: Santa goes to floor "
    print $ partOne input
    putStr "Part Two completed: The first index is "
    print $ partTwo input