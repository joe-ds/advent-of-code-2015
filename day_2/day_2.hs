import System.IO
import Data.List ( sort )

 -- Lazier than actually writing a split function.
interpret :: String -> [Integer]
interpret "" = []
interpret (c:'x':cs) = (read [c] :: Integer) : interpret cs
interpret (c1:c2:'x':cs) = (read [c1, c2] :: Integer) : interpret cs
interpret cs = [read cs :: Integer]

area :: [Integer] -> Integer
area [a,b,c] = (2*a*b) + (2*b*c) + (2*c*a)
area _ = error "Invalid input to area!"

partOne :: String -> Integer
partOne [] = 0
partOne cs = sum . map (f . interpret) . lines $ cs
            where
                f ns = (product . take 2 $ sort ns) + area ns

partTwo :: String -> Integer
partTwo [] = 0
partTwo cs = sum . map (f .interpret) . lines $ cs
            where
                f ns = ((2*) . sum . take 2 $ sort ns) + product ns

main :: IO ()
main = do
    f <- openFile "input" ReadMode
    input <- hGetContents  f
    putStr "Part One completed. Require "
    putStr . show . partOne $ input
    putStrLn " sq. ft."
    putStr "Part Two completed. Require "
    putStr . show . partTwo $ input
    putStrLn " ft."