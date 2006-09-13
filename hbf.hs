module Main where


import System
import Monad


data BrainF_ck = BF { bfPointer :: Int, bfRegister :: [Int] } deriving (Show)


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
                v <- getChar
                return $ BF p ((take p r) ++ [read [v]] ++ (tail $ drop p r))


bfPrint :: BrainF_ck -> IO BrainF_ck
bfPrint bf = do putStr $ show $ bfValue bf
                return bf


bfEvaluate :: BrainF_ck -> Char -> IO BrainF_ck
bfEvaluate bf c = case c of '+' -> return $ bfIncrement bf
                            '-' -> return $ bfDecrement bf
                            '>' -> return $ bfShift bf
                            '<' -> return $ bfUnshift bf
                            '.' -> bfPrint bf
                            ',' -> bfInput bf



main :: IO BrainF_ck
main = do args <- getArgs
          prog <- readFile $ head args
          foldM bfEvaluate bfInitial prog


