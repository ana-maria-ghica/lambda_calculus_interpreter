module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1. --------------------------------------------------------------------
-- returneaza toate variabilele din expresie
vars :: Lambda -> [String]
vars (Var s) = s : []
vars (App l1 l2) = nub (vars l1 ++ vars l2)  -- pentru concatenarea listelor: ++
vars (Abs str l) = nub (str : vars l)
vars (Macro m) = m : [] 


-- 1.2. --------------------------------------------------------------------
-- returneaza toate variabilele libere din expresie
freeVars :: Lambda -> [String]
freeVars (Var s) = s : []
freeVars (App l1 l2) = nub (freeVars l1 ++ freeVars l2)
freeVars (Abs str l) = freeVars l \\ [str]  -- pentru diferenta de multimi/ liste: \\ 
freeVars (Macro m) = m : []


-- 1.3. --------------------------------------------------------------------
newVar :: [String] -> String
newVar xs = head (filter (`notElem` xs) all_letters)
  where
    alphabet = map (:[]) ['a'..'z']
    alphabet_2 words = [a ++ b | a <- alphabet, b <- words]
    all_letters = concat (iterate alphabet_2 alphabet)  


-- 1.4. --------------------------------------------------------------------
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Abs _ e) = isNormalForm e
isNormalForm (Macro _) = True


-- 1.5. --------------------------------------------------------------------
reduce :: String -> Lambda -> Lambda -> Lambda
-- cazul 1: Var ---
reduce x (Var v1) v2
  | x == v1 = v2
  | otherwise = (Var v1)

-- cazul 2: Abs ---
reduce x (Abs y e1) e2 = 
  if y == x  -- se refera la ex: \x.(\x.(x x)  \y.z) -> unde (x x) apartin de al doilea x, nu de primul => shadowing!
    then Abs x e1
    else if elem y (freeVars e2)
      then Abs a (reduce x e1' e2) -- alpha renaming
      else Abs y (reduce x e1 e2) 

  where
    a = newVar (freeVars e2)  -- noua variabila cu care inlocuiesc
    e1' = reduce y e1 (Var a)  -- trebuie sa inlocuiesc si y in e1 cu a

-- cazul 3: App ---
reduce x (App e1 e2) exp = App (reduce x e1 exp) (reduce x e2 exp) 


-- 1.6. --------------------------------------------------------------------
normalStep :: Lambda -> Lambda
normalStep (Var x) = (Var x)
normalStep (Abs x e) = Abs x (normalStep e) -- pentru ca expresia e se poate reduce in interior!
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2) = App (normalStep e1) e2


-- 1.7. --------------------------------------------------------------------
applicativeStep :: Lambda -> Lambda
applicativeStep (Var x) = (Var x)
applicativeStep (Abs x e) = Abs x (applicativeStep e)

-- expl: isNormalForm e2 e exact intrebarea "e2 e complet evaluat?" - daca da, treci la reduce, daca nu, mai reduci un pas din e2
applicativeStep (App (Abs x e1) e2) =
  if isNormalForm e2
    then reduce x e1 e2
    else App (Abs x e1) (applicativeStep e2)

applicativeStep (App e1 e2) = App (applicativeStep e1) e2


-- 1.8. --------------------------------------------------------------------
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify f e =
  if isNormalForm e
    then [e]
    else e : (simplify f (f e))

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
