data Expr = Val Int | Div Expr Expr deriving Show

eval' :: Expr -> Int
eval' (Val n) = n 
eval' (Div x y) = (eval' x) `div` (eval' y)

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n m = Just (n `div` m)

eval'' :: Expr -> Maybe Int
eval'' (Val n) = Just n
eval'' (Div x y) = eval'' x >>= \n -> eval'' y >>= \m -> safediv n m
