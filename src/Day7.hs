module Day7 where

import Control.Monad.Identity
import Data.Maybe
import Data.Either
import Text.Parsec
import Data.List
import Data.Function

data C a = InL a | InR a deriving (Show, Eq)

isABBA (a:b:c:d:_) = a == d && b == c && a /= b
isABBA _           = False

hasABBA [] = False
hasABBA xxs@(x:xs)
  | isABBA xxs = True
  | otherwise  = hasABBA xs

str ::  Monad m => ParsecT String u m String
str = many1 lower

regularStr ::  Monad m => ParsecT String u m (String, String)
regularStr = str >>= \s -> return $! (s, mempty)

bracketStr ::  Monad m => ParsecT String u m (String, String)
bracketStr = between (char '[') (char ']') str >>= \s -> return $! (mempty, s)

data T2 a = T2 a a

parser :: Monad m => ParsecT String u m [(String, String)]
parser = many1 (choice [regularStr, bracketStr])

ex1 = "abba[mnop]qrst"
ex2 = "okorzkkyqucfigaauh[gpremtdkubmibdiiti]gsgtndjxpayklstcxkm[vrpprmlszcwqbfwbd]cbxuozoyhztdygegv[ojfihektxdbdrzdnk]rbxonpsewnfqikrfvp"

f (x, y) (k, (l, r))
  | odd  k = if null l then Nothing else Just (x || hasABBA l, y)
  | even k = if null r then Nothing else Just (x, y && (not . hasABBA) r)

unF = maybe False (uncurry (on (&&) id))

supportsTls :: String -> Bool
supportsTls t = case fmap (unF . foldM f (False, True) . zip [1..]) . runIdentity . runParserT parser 0 "<stdin>" $ t of
  Left _ -> False
  Right xs -> xs

tlsIps = length . filter id . map supportsTls . lines

main = getContents >>= print . tlsIps
