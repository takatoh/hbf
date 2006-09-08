module Main (main) where


import System


data BrainF_ck = BF { bfPointer :: Int, bfValues :: [Integer] } deriving (Show)


bfInitial :: BrainF_ck
bfInitial = BF { bfPointer = 0, bfValues = [0,0,0,0,0,0,0,0,0,0] }


bfValue :: BrainF_ck -> Integer
bfValue bf = (bfValues bf) !! (bfPointer bf)


bfIncrement :: BrainF_ck -> BrainF_ck
bfIncrement (BF p v) = BF p ((take p v) ++ [(v !! p) + 1] ++ (tail $ drop p v))


bfDecrement :: BrainF_ck -> BrainF_ck
bfDecrement (BF p v) = BF p ((take p v) ++ [(v !! p) - 1] ++ (tail $ drop p v))


bfShift :: BrainF_ck -> BrainF_ck
bfShift (BF p v) = BF (p+1) v


bfUnshift :: BrainF_ck -> BrainF_ck
bfUnshift (BF p v) = BF (p-1) v


bfInput :: BrainF_ck -> Integer -> BrainF_ck
bfInput (BF p v) n = BF p ((take p v) ++ [n] ++ (tail $ drop p v))


bfPrint bf = print $ bfValue bf


bfEvaluate = foldl eval
               where eval bf c = case c of
                                   '+' -> bfIncrement bf
                                   '-' -> bfDecrement bf
                                   '>' -> bfShift bf
                                   '<' -> bfUnshift bf



main = do args <- getArgs
          prog <- readFile $ head args
          print $ bfEvaluate bfInitial prog


