module Code where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1. ---------------------------------------
replace :: Context -> Lambda -> Either String Lambda
replace ctx (Var x) = Right (Var x)
replace ctx (Abs x e) = 
    case replace ctx e of
        Left val -> Left val
        Right val -> Right (Abs x val)
        
replace ctx (App e1 e2) =
    case replace ctx e1 of
        Left val1 -> Left val1 -- propag eroarea mai departe!
        Right val1 -> case replace ctx e2 of
            Left val2 -> Left val2 -- propag eroarea mai departe! 
            Right val2 -> Right (App val1 val2)

replace ctx (Macro m) = 
    case lookup m ctx of
        Nothing -> Left m
        Just val -> Right val

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx f lambda = 
    case replace ctx lambda of
        Left val -> Left val
        Right val -> Right (simplify f val)

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
