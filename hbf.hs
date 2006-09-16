module Main where


import System
import Monad
import Data.Char


data BrainF_ck = BF { bfPointer :: Int, bfRegister :: [Int] } deriving (Show)

type Program = ([Char], [Char])


bfInitial :: BrainF_ck
bfInitial = BF { bfPointer = 0, bfRegister = [0,0,0,0,0,0,0,0,0,0] }


bfValue :: BrainF_ck -> Int
bfValue bf = (bfRegister bf) !! (bfPointer bf)


bfIncrement :: BrainF_ck -> BrainF_ck
bfIncrement (BF p v) = BF p ((take p v) ++ [(v !! p) + 1] ++ (tail $ drop p v))


bfDecrement :: BrainF_ck -> BrainF_ck
bfDecrement (BF p v) = BF p ((take p v) ++ [(v !! p) - 1] ++ (tail $ drop p v))


bfShift :: BrainF_ck -> BrainF_ck
bfShift (BF p v) = BF (p+1) v


bfUnshift :: BrainF_ck -> BrainF_ck
bfUnshift (BF p v) = BF (p-1) v


bfInput :: BrainF_ck -> IO BrainF_ck
bfInput bf = do let p = bfPointer bf
                let r = bfRegister bf
                putStr "\ninput? "
                c <- getChar
                return $ BF p ((take p r) ++ [ord c] ++ (tail $ drop p r))


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
progFetch prog = snd prog


progShift :: Program -> Program
progShift (l, (r:rs)) = (l ++ [r], rs)


progUnshift :: Program -> Program
progUnshift (l, r) = (take ((length l) -1) l, (last l):r)


progSkip :: BrainF_ck -> Program -> Program
progSkip bf prog = if bfValue bf == 0 then skip prog else progShift prog
  where skip p = if (head $ progFetch next) == ']' then next else skip next
          where next = progShift p


progBack :: BrainF_ck -> Program -> Program
progBack bf prog = if bfValue bf == 0 then progShift prog else back prog
  where back p = if (head $ progFetch prev) == '[' then prev else back prev
          where prev = progUnshift p


bfRun :: BrainF_ck -> Program -> IO BrainF_ck
bfRun bf prog = run $ progFetch prog
  where run []                  = return bf
        run (c:cs) | c == '['   = bfRun bf (progSkip bf prog)
                   | c == ']'   = bfRun bf (progBack bf prog)
                   | otherwise = do next <- bfEvaluate bf c
                                    bfRun next (progShift prog)


main :: IO BrainF_ck
main = do filename <- getArgs >>= return . head
          prog <- readFile filename >>= ((return . progNew) . concat) . lines
          bfRun bfInitial prog


