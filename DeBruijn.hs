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
-- functia de shiftare
shift :: Int -> DeBruijn -> DeBruijn
shift 0 e = e
shift n (DBVar x) = shift (n-1) (DBVar (x+1))
shift n (DBFree x) = DBFree x
shift n (DBApp e1 e2) = DBApp (shift n e1) (shift n e2)
shift n (DBAbs str e) = DBAbs str (shift n e)

-- functia de reducere
reduce :: DeBruijn -> DeBruijn -> DeBruijn
reduce param e = reduce' 0 param e
    where
        reduce' depth param (DBVar x) =
            if (x == depth) then shift depth param
            else if (x > depth) then (DBVar (x-1))
            else (DBVar x)

        reduce' depth param (DBFree x) = DBFree x

        reduce' depth param (DBApp e1 e2) = DBApp (reduce' depth param e1) (reduce' depth param e2)

        reduce' depth param (DBAbs x e) = DBAbs x (reduce' (depth+1) param e)


-- 4.5. ---------------------------------------------
normalStep :: DeBruijn -> DeBruijn
normalStep (DBVar x) = DBVar x
normalStep (DBFree x) = DBFree x
normalStep (DBAbs x e) = DBAbs x (normalStep e)
normalStep (DBApp (DBAbs x e1) e2) = reduce e2 e1 -- aici se aplica reducerea efectiva, in rest sunt apeluri recursive
normalStep (DBApp e1 e2) = DBApp (normalStep e1) (normalStep e2)

-- 4.6. ---------------------------------------------
applicativeStep :: DeBruijn -> DeBruijn
applicativeStep (DBVar x) = DBVar x
applicativeStep (DBFree x) = DBFree x
applicativeStep (DBAbs x e) = DBAbs x (applicativeStep e)
applicativeStep (DBApp (DBAbs x e1) e2) = 
    if isNormalForm e2 
        then reduce e1 e2
        else DBApp (DBAbs x e1) (applicativeStep e2)
applicativeStep (DBApp e1 e2) = DBApp e1 (applicativeStep e2)

-- 4.7. ---------------------------------------------
simplify :: (DeBruijn -> DeBruijn) -> DeBruijn -> [DeBruijn]
simplify f exp =
    if isNormalForm exp
        then [exp]
        else exp : (simplify f (f exp))

normal :: DeBruijn -> [DeBruijn]
normal = simplify normalStep

applicative :: DeBruijn -> [DeBruijn]
applicative = simplify applicativeStep