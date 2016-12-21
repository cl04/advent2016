import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Tuple
import           Data.Char
import           Text.Parsec
import           Data.Monoid
import           Data.List
import           Control.Monad.Identity
import           Debug.Trace

checkCsum s = (== csum s)
  where csum = take 5 . concat . Map.elems . Map.fromListWith (flip (++))  . map (fmap pure) . map swap . Map.assocs . Map.fromListWith (+) . flip zip (repeat (-1))

parser :: Monad m => ParsecT String u m (Either String Int)
parser = do
  chrs <- sepEndBy (many1 lower) (char '-')
  room <- many1 digit
  csum <- between (char '[') (char ']') (count 5 lower)
  if checkCsum (concat chrs) csum then return (Right (read room)) else return (Left ("bad checksum: " ++ csum))

rot1 x
  | x >= 'a' && x < 'z' = succ x
  | x == 'z'            = 'a'
  | x == '-' = ' '
  | x == ' ' = ' '
rot k x
  | k > 26 = rot (k `mod` 26) x
  | k == 0 = x
  | otherwise = rot (k-1) (rot1 x)

decrypt = map rot

parse_ t = case (runIdentity . runParserT parser 0 "<stdin>") t of
  Left _ -> mempty
  Right m -> case m of
    Left _ -> mempty
    Right v -> Sum v

parseDecrypt f t = case (runIdentity . runParserT parser 0 "<stdin>") t of
  Left _ -> mempty
  Right m -> case m of
    Left _ -> mempty
    Right v -> if f (map (rot v) (takeWhile (not . isDigit) t)) then (First (Just v)) else mempty
    
roomSum = foldMap parse_ . lines

northPoleSectorId = getFirst . foldMap (parseDecrypt pred) . lines
  where pred s = "northpole object" `isPrefixOf` s

