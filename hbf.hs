module Main where


import System
import Monad
import Data.Char


data BrainF_ck = BF { bfPointer :: Int, bfRegister :: [Int] } deriving (Show)

type Program = ([Char], [Char])


bfInitial :: BrainF_ck
bfInitial = BF { bfPointer = 0, bfRegister = replicate 32 0 }


bfValue :: BrainF_ck -> Int
bfValue bf = (bfRegister bf) !! (bfPointer bf)


bfSetValue :: BrainF_ck -> Int -> BrainF_ck
bfSetValue (BF p v) i = BF p ((take p v) ++ [i] ++ (tail $ drop p v))


bfIncrement :: BrainF_ck -> BrainF_ck
bfIncrement bf = bfSetValue bf (bfValue bf + 1)


bfDecrement :: BrainF_ck -> BrainF_ck
bfDecrement bf = bfSetValue bf (bfValue bf - 1)


bfShift :: BrainF_ck -> BrainF_ck
bfShift (BF p v) = BF (p+1) v


bfUnshift :: BrainF_ck -> BrainF_ck
bfUnshift (BF p v) = BF (p-1) v


bfInput :: BrainF_ck -> IO BrainF_ck
bfInput bf = do putStr "\ninput? "
                c <- getChar
                return $ bfSetValue bf (ord c)


bfPrint :: BrainF_ck -> IO BrainF_ck
bfPrint bf = do putChar $ chr $ bfValue bf
                return bf


bfEvaluate :: BrainF_ck -> Char -> IO BrainF_ck
bfEvaluate bf '+' = return $ bfIncrement bf
bfEvaluate bf '-' = return $ bfDecrement bf
bfEvaluate bf '>' = return $ bfShift bf
bfEvaluate bf '<' = return $ bfUnshift bf
bfEvaluate bf '.' = bfPrint bf
bfEvaluate bf ',' = bfInput bf


progNew :: String -> Program
progNew str = ([], str)


progFetch :: Program -> [Char]
progFetch = snd


progShift :: Program -> Program
progShift (l, (r:rs)) = (l ++ [r], rs)


progUnshift :: Program -> Program
progUnshift (l, r) = (take ((length l) -1) l, (last l):r)


progSkip :: BrainF_ck -> Program -> Program
progSkip bf | bfValue bf == 0 = skip
            | otherwise       = progShift
  where
    skip p | (head . progFetch) next == ']' = next
           | otherwise                      = skip next
      where
        next = progShift p


progBack :: BrainF_ck -> Program -> Program
progBack bf | bfValue bf == 0 = progShift
            | otherwise       = back
  where
    back p | (head . progFetch) prev == '[' = prev
           | otherwise                      = back prev
      where
        prev = progUnshift p


bfRun :: BrainF_ck -> Program -> IO ()
bfRun bf prog = (run . progFetch) prog
  where
    run []                 = return ()
    run (c:cs) | c == '['  = bfRun bf $ progSkip bf prog
               | c == ']'  = bfRun bf $ progBack bf prog
               | otherwise = do next <- bfEvaluate bf c
                                bfRun next $ progShift prog


main :: IO ()
main = do filename <- getArgs >>= return . head
          prog <- readFile filename >>= return . progNew . concat . lines
          bfRun bfInitial prog


