module DeBruijn where

import Lambda (Lambda(..))
import Data.List
type Context = [String]

data DeBruijn = DBVar Int
              | DBFree String
              | DBApp DeBruijn DeBruijn
              | DBAbs String DeBruijn

instance Show DeBruijn where
    show (DBVar n) = show n
    show (DBFree x) = x
    show (DBApp e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (DBAbs _ e) = "λ " ++ show e

instance Eq DeBruijn where
    (DBVar n) == (DBVar m) = n == m
    (DBFree _) == (DBFree _) = True
    (DBApp e1 e2) == (DBApp f1 f2) = e1 == f1 && e2 == f2
    (DBAbs _ e) == (DBAbs _ f) = e == f
    _ == _ = False

-- 4.1. ---------------------------------------------
toDB :: Context -> Lambda -> DeBruijn
toDB ctx (Var x) =
    case elemIndex x ctx of
        Nothing -> DBFree x
        Just value -> DBVar value
toDB ctx (Abs x e) = DBAbs x (toDB (x:ctx) e)
toDB ctx (App e1 e2) = DBApp (toDB ctx e1) (toDB ctx e2)
toDB ctx (Macro m) = DBFree m

-- 4.2. ---------------------------------------------
fromDB :: Context -> DeBruijn -> Lambda
fromDB ctx (DBFree x) = Var(x)
fromDB ctx (DBVar x) = Var(ctx !! x) -- !! operatorul de indexare
fromDB ctx (DBAbs str dbj) = Abs str (fromDB (str:ctx) dbj)
fromDB ctx (DBApp dbj1 dbj2) = App (fromDB ctx dbj1) (fromDB ctx dbj2)  

-- 4.3. ---------------------------------------------
isNormalForm :: DeBruijn -> Bool
isNormalForm (DBVar x) = True
isNormalForm (DBFree x) = True
isNormalForm (DBApp (DBAbs str dbj1) dbj2) = False
isNormalForm (DBApp dbj1 dbj2) = isNormalForm dbj1 && isNormalForm dbj2 
isNormalForm (DBAbs str dbj) = isNormalForm dbj

-- 4.4. ---------------------------------------------
shifting :: DeBruijn -> DeBruijn
shifting (DBVar x) = DBVar (x+1)
shifting (DBFree x) = DBFree x
shifting (DBApp e1 e2) = DBApp (shifting e1) (shifting e2)
shifting (DBAbs str e) = DBAbs str (shifting e)

reduce :: DeBruijn -> DeBruijn -> DeBruijn -- -----------------------------------------------------de reluat neaparat
reduce body (DBVar 0) = body
reduce body (DBVar x) = DBVar (x-1)
reduce body (DBFree x) = DBFree x
reduce body (DBApp dbj1 dbj2) = DBApp (reduce body dbj1) (reduce body dbj2)
reduce body (DBAbs str e) = DBAbs str (reduce (shifting body) e)

-- reduce :: DeBruijn -> DeBruijn -> DeBruijn
-- reduce body e = reduceAt 0 body e
--   where
--     reduceAt depth body (DBVar x)
--         | x == depth  = shifting_n depth body
--         | x > depth   = DBVar (x-1)
--         | otherwise   = DBVar x
--     reduceAt depth body (DBFree x) = DBFree x
--     reduceAt depth body (DBApp e1 e2) = DBApp (reduceAt depth body e1) (reduceAt depth body e2)
--     reduceAt depth body (DBAbs str e) = DBAbs str (reduceAt (depth+1) body e)

--     shifting_n 0 e = e
--     shifting_n n e = shifting_n (n-1) (shifting e)

-- 4.5.
normalStep :: DeBruijn -> DeBruijn
normalStep (DBVar x) = DBVar x
normalStep (DBFree x) = DBFree x
normalStep (DBAbs x e) = DBAbs x (normalStep e)
normalStep (DBApp (DBAbs x e1) e2) = reduce e2 e1 -- aici se aplica reducerea efectiva, in rest sunt apeluri recursive
normalStep (DBApp e1 e2) = DBApp (normalStep e1) (normalStep e2)

-- 4.6.
applicativeStep :: DeBruijn -> DeBruijn
applicativeStep (DBVar x) = DBVar x
applicativeStep (DBFree x) = DBFree x
applicativeStep (DBAbs x e) = DBAbs x (applicativeStep e)
applicativeStep (DBApp (DBAbs x e1) e2) = 
    if isNormalForm e2 
        then reduce e1 e2
        else DBApp (DBAbs x e1) (applicativeStep e2)
applicativeStep (DBApp e1 e2) = DBApp e1 (applicativeStep e2)

-- 4.7.
simplify :: (DeBruijn -> DeBruijn) -> DeBruijn -> [DeBruijn]
simplify f exp =
    if isNormalForm exp
        then [exp]
        else exp : (simplify f (f exp))

normal :: DeBruijn -> [DeBruijn]
normal = simplify normalStep

applicative :: DeBruijn -> [DeBruijn]
applicative = simplify applicativeStep
