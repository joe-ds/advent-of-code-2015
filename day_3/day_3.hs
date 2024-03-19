import System.IO

type Coord = (Integer, Integer)

newCoord :: Char -> Coord -> Coord
newCoord '^' (x, y) = (x, y+1)
newCoord 'v' (x, y) = (x , y-1)
newCoord '>' (x, y) = (x+1, y)
newCoord '<' (x, y) = (x-1, y)
newCoord _   cs     = cs

traveller :: String -> [Coord] -> Coord -> Integer
traveller "" hs _ = toInteger . length $ hs 
traveller (c:cs) past current = traveller cs past' current'
                                where
                                    current' = newCoord c current
                                    past' = if current' `elem` past then past
                                            else current' : past

partOne :: String -> Integer
partOne cs = traveller cs [(0, 0)] (0, 0)

santa :: String -> [Coord] -> Coord -> Coord -> Integer
santa "" past _ _ = toInteger . length $ past
santa (c:cs) past sc rc = robo cs past' sc' rc
                                where
                                    sc' = newCoord c sc
                                    past' = if sc' `elem` past then past
                                            else sc' : past

robo :: String -> [Coord] -> Coord -> Coord -> Integer
robo "" past _ _ = toInteger . length $ past
robo (c:cs) past sc rc = santa cs past' sc rc'
                                where
                                    rc' = newCoord c rc
                                    past' = if rc' `elem` past then past
                                            else rc' : past

partTwo :: String -> Integer
partTwo cs = santa cs [(0, 0)] (0, 0) (0, 0)

main :: IO ()
main = do
    f <- openFile "input" ReadMode
    input <- hGetContents  f
    putStr "Part One completed. "
    putStr . show . partOne $ input
    putStrLn " houses visited."
    putStr "Part Two completed. "
    putStr . show . partTwo $ input
    putStrLn " houses visited."