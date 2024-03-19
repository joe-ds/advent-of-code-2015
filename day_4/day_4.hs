import System.IO
import qualified Data.ByteString.Char8 as BC
import qualified Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16

-- DEPENDENCIES --------------------------------------------------------------
------------------------------------------------------------------------------
-- cryptohash-md5 [https://hackage.haskell.org/package/cryptohash-md5]
-- base16-bytestring [https://hackage.haskell.org/package/base16-bytestring]

-- BUILDING ------------------------------------------------------------------
------------------------------------------------------------------------------
-- Compiled with 
-- gch day_4.hs -o day_4 -O2
-- 
-- The performance is sweet!

input :: BC.ByteString
input = BC.pack "ckczppom"

partOne :: Integer
partOne = head $ dropWhile f [1..]
            where
                f x = BC.pack "00000" /= BC.take 5 (h x)
                h n = encode . MD5.hash $ input <> (BC.pack . show $ n)

partTwo :: Integer
partTwo = head $ dropWhile f [1..]
            where
                f x = BC.pack "000000" /= BC.take 6 (h x)
                h n = encode . MD5.hash $ input <> (BC.pack . show $ n)

main = do
    putStrLn $ "Part One completed. The answer is " ++ show partOne ++ "."
    putStrLn $ "Part Two completed. The answer is " ++ show partTwo ++ "."
