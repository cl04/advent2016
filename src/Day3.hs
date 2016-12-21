module Day3 where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)
import           Data.Maybe
import           Data.Char

data Unvalidated
data Validated

data P3 a = P3 !Int !Int !Int deriving (Show, Eq)

validate :: P3 Unvalidated -> Maybe (P3 Validated)
validate (P3 x y z)
  | x + y > z && x + z > y && y + z > x = Just (P3 x y z)
  | otherwise = Nothing

readP3 :: ByteString -> P3 Unvalidated
readP3 s = fromJust $ (C.readInt . (C.dropWhile isSpace)) s >>= \(x, s1) ->
  (C.readInt . (C.dropWhile isSpace)) s1 >>= \(y, s2) ->
  (C.readInt . (C.dropWhile isSpace)) s2 >>= \(z, _) ->
  return (P3 x y z)

readP3s :: [ByteString] -> [P3 Unvalidated]
readP3s bs
  | null bs = []
  | otherwise = P3 x1 y1 z1 : P3 x2 y2 z2 : P3 x3 y3 z3 : readP3s bs'
  where (l1:l2:l3:_, bs') = splitAt 3 bs
        P3 x1 x2 x3     = readP3 l1
        P3 y1 y2 y3     = readP3 l2
        P3 z1 z2 z3     = readP3 l3

validTriangles = length . filter isJust . map (validate . readP3) . C.lines
validTriangles2 = length . filter isJust . map validate . readP3s . C.lines
