module Main where

import Data.List (isInfixOf)
import Data.Char (chr, ord)
import Data.Word
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

md5 = B.unpack . MD5.hash . C8.pack
md5s = toHex . map fromIntegral . md5

z :: Word8
z = fromIntegral $ ord '0'

a :: Word8
a = fromIntegral $ ord 'a'

md5p bs = B.concatMap (\xy -> let (x,y) = xy `divMod` 16 in B.pack [h x, h y]) (MD5.hash bs)
  where h d | d < 10 = d + z
        h d          = d - 10 + a


toHex [] = ""
toHex (w:ws) = h (w `div` 16) : h (w `mod` 16) : toHex ws
  where h d | d < 10 = chr (d + ord '0')
        h d          = chr (d - 10 + ord 'a')

--hashes :: Int -> String -> Int -> [(String, Int)]
hashes' rep salt index = rec index
  where rec index = (f rep (C8.pack $ salt ++ show index), index) : rec (index+1)
        f 0 s = s
        f r s = f (r-1) (md5p s)

hashes rep salt index = [ (B.unpack s, i) | (s,i) <- hashes' rep salt index ]

genKeys rep salt index = filterCandidates (hashes rep salt index)

filterCandidates ((k,i):ks) =
  case getTriplet k of
    Just c -> (if any (replicate 5 c `isInfixOf`) (map fst $ take 1000 ks) then [(k,i)] else []) ++ filterCandidates ks
    _      -> filterCandidates ks

getTriplet (a:b:c:ds) | a == b && b == c        = Just a
                      | otherwise               = getTriplet (b:c:ds)
getTriplet _ = Nothing

salt = "jlmsuwbz"

result1 = take 64 $ genKeys 1 salt 0
result2 = take 64 $ genKeys 2017 salt 0


getTriplet' :: B.ByteString -> [Word8]
getTriplet' bs = [ B.head g | g <- B.group bs, B.length g >= 3 ]

filterCand ((k,i):ks) =
  case getTriplet' k of
    (c:_) -> (if any (B.replicate 5 c `B.isInfixOf`) (map fst $ take 1000 ks) then [(k,i)] else []) ++ filterCand ks
    _     -> filterCand ks

main = mapM_ print result2
