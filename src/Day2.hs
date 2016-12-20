{-# LANGUAGE FlexibleContexts #-}

module Day2 where

import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe

type KeyMap = (Int, (Int, Int, Int, Int))

keypadMap1 :: [ KeyMap ]
keypadMap1 = [ (1, (1, 2, 4, 1))
             , (2, (2, 3, 5, 1))
             , (3, (3, 3, 6, 2))
             , (4, (1, 5, 7, 4))
             , (5, (2, 6, 8, 4))
             , (6, (3, 6, 9, 5))
             , (7, (4, 8, 7, 7))
             , (8, (5, 9, 8, 7))
             , (9, (6, 9, 9, 8)) ]
--     1
--   2 3 4
-- 5 6 7 8 9
--   A B C
--     D
keypadMap2 :: [ KeyMap ]
keypadMap2 = [   (1, (1, 1, 3, 1))  , (2, (2, 3, 6, 2))
              , (3, (1, 4, 7, 2))  , (4, (4, 4, 8, 3))
              , (5, (5, 6, 5, 5))  , (6, (2, 7, 10, 5))
              , (7, (3, 8, 11, 6))  , (8, (4, 9, 12, 7))
              , (9, (9, 9, 9, 8))  , (10, (6, 11, 10, 10))
              , (11, (7, 12, 13, 10)), (12, (8, 12, 12, 11))
              , (13, (11, 13, 13, 13)) ]

data Dir = U | R | D | L deriving (Eq, Show, Read)

move :: MonadReader [KeyMap] m => Int -> Dir -> m Int
move x dir = asks (maybe x (select dir) . lookup x)
  where
    select U (u, _, _, _) = u
    select R (_, r, _, _) = r
    select D (_, _, d, _) = d
    select L (_, _, _, l) = l

moves_ :: (MonadState Int m, MonadReader [KeyMap] m) => [Dir] -> m Int
moves_ xs = do
  p <- get
  x' <- foldM move p xs
  put x'
  return x'
  
keycodes :: [KeyMap] -> [[Dir]] -> [Int]
keycodes km dirs = runReader (evalStateT (mapM moves_ dirs) 5) km
