module Main where


import System


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


bfInput :: BrainF_ck -> Int -> BrainF_ck
bfInput (BF p v) n = BF p ((take p v) ++ [n] ++ (tail $ drop p v))


bfPrint bf = do print $ bfValue bf
                return bf


bfEvaluate bf c = do bf' <- bf
                     case c of '+' -> return $ bfIncrement bf'
                               '-' -> return $ bfDecrement bf'
                               '>' -> return $ bfShift bf'
                               '<' -> return $ bfUnshift bf'
                               '.' -> bfPrint bf'



--main = do args <- getArgs
--          prog <- readFile $ head args
--          bf <- foldl bfEvaluate bfInitial prog
--          print bf
--
