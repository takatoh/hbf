module Main (main) where


import System


data BrainFuck = BF { bfPointer :: Int, bfValues :: [Integer] } deriving (Show)


bfInitial :: BrainFuck
bfInitial = BF { bfPointer = 0, bfValues = [0,0,0,0,0,0,0,0,0,0] }


bfValue :: BrainFuck -> Integer
bfValue bf = (bfValues bf) !! (bfPointer bf)


bfIncrement :: BrainFuck -> BrainFuck
bfIncrement (BF p v) = BF p ((take p v) ++ [(v !! p) + 1] ++ (tail $ drop p v))


bfDecrement :: BrainFuck -> BrainFuck
bfDecrement (BF p v) = BF p ((take p v) ++ [(v !! p) - 1] ++ (tail $ drop p v))


bfShift :: BrainFuck -> BrainFuck
bfShift (BF p v) = BF (p+1) v


bfUnshift :: BrainFuck -> BrainFuck
bfUnshift (BF p v) = BF (p-1) v


bfInput :: BrainFuck -> Integer -> BrainFuck
bfInput (BF p v) n = BF p ((take p v) ++ [n] ++ (tail $ drop p v))


bfPrint bf = print $ bfValue bf


bfEvaluate = foldl eval
               where eval bf' c = case c of
                                    '+' -> bfIncrement bf'
                                    '-' -> bfDecrement bf'
                                    '>' -> bfShift bf'
                                    '<' -> bfUnshift bf'



main = do args <- getArgs
          prog <- readFile $ head args
          print $ bfEvaluate bfInitial prog


