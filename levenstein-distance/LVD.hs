{-# LANGUAGE FlexibleContexts #-}

-- Functions for computing `Levenshtein distance` between two strings
-- This module shows few approaches to compute it - from 
-- the simplest to more advanced ones.

import Control.Monad.Memo
import Data.List
import System.Random


main :: IO ()
main = do
        -- randomly generate 2 strings
        gen1 <- getStdGen
        let randWord1 = take 40 (randomRs ('a', 'f') gen1)
        gen2 <- newStdGen
        let randWord2 = take 40 (randomRs ('a', 'f') gen2)

        putStrLn $ "1st word: " ++ randWord1
        putStrLn $ "2nd word: " ++ randWord2
        putStrLn $ "LVD (with memo): " 
          ++ show ( startEvalMemo (memoLvd (randWord1, randWord2)))
        -- BOOM! too slow... (could easily take a day to finish :) )
        --putStrLn $ "LVD: " ++ show ( lvd randWord1 randWord2 )
        putStrLn $ "LVD (table approach): " 
          ++ show (lvdTable randWord1 randWord2)


-- the slowest version (but simple)
lvd :: String -> String -> Int
lvd [] s2 = length s2
lvd s1 [] = length s1
lvd s1@(x1:xs1) s2@(x2:xs2) = 
    let cost 
          | x1 == x2  = 0
          | otherwise = 1
    in  min (min (lvd xs1 s2 + 1)
                 (lvd s1 xs2 + 1))
            (lvd xs1 xs2 + cost)


-- memoized (faster) version of the previous recursive algorithm
-- (but consume much more memory), uses Memo monad from Control.Monad.Memo
memoLvd :: (String, String) -> Memo (String, String) Int Int
memoLvd ([],s2) = return $ length s2
memoLvd (s1,[]) = return $ length s1
memoLvd (s1@(x1:xs1), s2@(x2:xs2)) = do
                                      a <- memo memoLvd (xs1, s2) -- caching
                                      b <- memo memoLvd (s1, xs2)
                                      c <- memo memoLvd (xs1, xs2)
                                      let cost = if x1 == x2 then 0 else 1
                                      return $ min (min (a+1) (b+1)) (c+cost)



-- lazily computes distance, constructing table of prefixes
-- using Dynamic Programming approach
lvdTable :: String -> String -> Int
lvdTable str1 str2 = rows !! l2 !! l1

  where l1 = length str1
        l2 = length str1
        startColumn = [0..(length str1)]
        startRow    = [0..(length str2)]

        -- list of rows defined using corecursion (self-referencing), i.e
        -- each row (but first) is defined in terms of the previous one
        rows = startColumn : zipWith3 apply2 (map makeNextColumn rows)
                                             (tail startRow)
                                             str2
        apply2 f a b = f a b
        min3 a b c = min (min a b) c
        minIf a b c cond = min3 (a + 1) (b + 1) 
                                (if cond then c else c + 1)
        -- we define each row as minimum of itself and the previous row 
        -- (shifted upward by 1 and 0), so altogether we have 3 rows and 
        -- we're able to find the value of the current cell in the row 
        -- by computing the minimum of the 3 adjacent cells
        plusOneTest symb = map (symb ==) str1
        -- the forth row (plusOneTest) is ised for testing
        -- for the equality of the 2 current chars of input strings
        -- for each cell in the row
        makeNextColumn prevRow start symb = 
          let currRow = start : zipWith4 minIf 
                                currRow (tail prevRow) 
                                prevRow (plusOneTest symb)
          in  currRow


