{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.IntMap.Strict as IntMap
import           Data.IntMap.Strict (IntMap)
import           Control.Monad.State
import           Numeric

import Crypto.Hash
import Data.List
import Debug.Trace

numDigits x
  | x == 0 = 0
  | x < 10 = 1
  | x >= 10 = 1 + numDigits (x `div` 10)

nextHelper :: Integer -> ByteString -> (Char, Integer)
nextHelper i s
  | "00000" `isPrefixOf` m = (head . drop 5 $ m, i)
  | otherwise = nextHelper (1+i) s
  where
    t = C.concat [s, pad, C.pack (show i)]
    pad = C.replicate (5 - numDigits i) '0'
    h = (hash t :: Digest MD5)
    m = show h

next :: (String, Integer) -> ByteString -> (String, Integer)
next (z, i) s = (z ++ [c], 1+i')
  where (c, i') = nextHelper i s

next' r i s
  | not ("00000" `isPrefixOf` msg) = next' r (1+i) s
  | IntMap.size r == 8 = r
  | pos `IntMap.member` r  || pos >= 8 = next' r (1+i) s
  | pos `IntMap.notMember` r = next' (IntMap.insert pos ch r) (1+i) s
  where
    t = C.concat [s, pad, C.pack (show i)]
    pad = C.replicate (5 - numDigits i) '0'
    h = (hash t :: Digest MD5)
    msg = show h
    (pos_:ch:_) = drop 5 msg
    pos = fst . head . readHex . pure $ pos_

password :: ByteString -> ByteString
password = C.pack . fst . foldl next ([], 0) . replicate 8

password' :: ByteString -> ByteString
password' = C.pack . IntMap.elems . next' IntMap.empty 0 
