import System.IO
import Text.Regex.PCRE

-- DEPENDENCIES --------------------------------------------------------------
------------------------------------------------------------------------------
-- regex-pcre-builtin [https://hackage.haskell.org/package/regex-pcre-builtin]

rule1 :: String -> Bool 
rule1 s = (s =~ "[aeiou]" :: Int) >= 3

rule2 :: String -> Bool 
rule2 s = s =~ "([a-z])\\1" :: Bool

rule3 :: String -> Bool
rule3 s = not $ s =~ "ab|cd|pq|xy" :: Bool

partOne :: String -> Integer
partOne "" = 0
partOne s  = toInteger . length . filter (== True) . map applyRules $ lines s
            where
                applyRules s = all ((== True) . ($ s)) [rule1, rule2, rule3]

rule4 :: String -> Bool 
rule4 s = s =~ "([a-z]{2}).*?\\1" :: Bool

rule5 :: String -> Bool 
rule5 s = s =~ "([a-z]).\\1" :: Bool

partTwo :: String -> Integer
partTwo "" = 0
partTwo s  = toInteger . length . filter (== True) . map applyRules $ lines s
            where
                applyRules s = all ((== True) . ($ s)) [rule4, rule5]

main :: IO ()
main = do
    f <- openFile "input" ReadMode
    input <- hGetContents  f
    putStrLn $ "Part One completed. " ++ show (partOne input) ++ " nice strings."
    putStrLn $ "Part Two completed. " ++ show (partTwo input) ++ " nice strings."