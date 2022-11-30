module Foo where

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Char (chr, ord)
import Data.List (isPrefixOf)
import Data.Word (Word8)
import Data.Maybe (isJust)

md5 = B.unpack . MD5.hash . C8.pack
md5s = toHex . map fromIntegral . md5

toHex [] = ""
toHex (w:ws) = h (w `div` 16) : h (w `mod` 16) : toHex ws
  where h d | d < 10 = chr (d + ord '0')
        h d          = chr (d - 10 + ord 'a')


hashes input = map md5 [ input ++ show i | i <- [0..] ]

filteredHashes = filter validHash . hashes

validHash :: [Word8] -> Bool
validHash (0:0:c:_) = c < 16
validHash _ = False

textHashes = map (toHex . map fromIntegral) . filteredHashes

code = concatMap (take 1 . drop 5) . take 8 . textHashes



code2 = code2' (replicate 8 Nothing) . textHashes

code2' :: [Maybe Char] -> [String] -> String
code2' code _ | all isJust code    = [ c | Just c <- code ]
code2' code (h:hs)                 = code2' code' hs
  where ('0':'0':'0':'0':'0':pc:cc:_) = h
        p = read (['0','x',pc]) :: Int
        code' = [ if i == p then Just (maybe cc id c) else c | (c,i) <- zip code [0..] ]
