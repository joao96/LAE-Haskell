module F4LAE where 

import Prelude hiding (lookup)
import Data.List hiding (lookup)
import System.IO hiding (lookup)

type Name = String  
type FormalArg = String 
type Id = String 

data FunDec = FunDec Name [FormalArg] Exp 

data Exp = Num Integer
         | Add Exp Exp 
         | Sub Exp Exp 
         | Ref Id
         | Trans Exp
         | Let Id Exp Exp 
         | App Name [Exp] 
         | Lambda FormalArg Exp
         | IfZero Exp Exp Exp
         | AppLambda Exp Exp
     deriving(Show, Eq)     

type DefrdSub = [ (Id, Value) ] 

data Value = NumValue Integer
           | Closure FormalArg Exp DefrdSub  
    deriving(Show, Eq)

interp :: Exp -> DefrdSub -> [FunDec] -> Value

tExp :: [Exp] -> DefrdSub -> [FunDec] -> [Value]
tExp [] _ _ = []
tExp _ _ [] = []
tExp (x : xs) lsub decs = (interp x lsub decs) : tExp xs lsub decs


interp (Num n)  _ _                = NumValue n
interp (Add l r) lsub decs         = binOperation (+) l r lsub decs 
interp (Sub l r) lsub decs         = binOperation (-) l r lsub decs

interp (Trans expLet) lsub decs    =
  let (Let x e1 e2) = expLet
    in (interp (AppLambda (Lambda x e2) e1) lsub decs)

interp (Let x e1 e2) lsub decs     = 
    interp e2 ((x, interp e1 lsub decs) : lsub) decs

interp (Ref v) lsub decs           = 
  let tup = lookupSub v lsub
  in case tup of
     (Nothing) -> error "Reference not found"
     (Just (  (v, value) : [] ) ) -> value 

interp (Lambda fArg corpo) lsub decs = Closure fArg corpo lsub

interp (AppLambda e1 e2) lsub decs  =
  let (Closure fArg corpo lscopo) = interp e1 lsub decs
    in interp corpo ((fArg, interp e2 lscopo decs): lscopo) decs

interp (App name lexpNom) lsub decs =
  let f = lookup name decs
   in case f of
    (Nothing) -> error "Function not declared"
    (Just (FunDec n lfArg corpo)) -> interp corpo ( (zip lfArg (tExp lexpNom lsub decs)) ++ lsub) decs


interp (IfZero cond t e) lsub decs
  | interp cond lsub decs == (NumValue 0) = interp t lsub decs
  | otherwise = interp e lsub decs

lookup :: Name -> [FunDec] -> Maybe FunDec
lookup _ [] = Nothing 
lookup f (fun@(FunDec n a b):fs)
  | f == n = Just fun
  | otherwise = lookup f fs 

lookupSub :: Id -> DefrdSub -> Maybe DefrdSub

lookupSub _ [] = Nothing 
lookupSub idSub (tuple@((n, value)) : rs)
  | idSub == n = Just (tuple : [])
  | otherwise = lookupSub idSub rs 

binOperation :: (Integer -> Integer -> Integer) -> Exp -> Exp -> DefrdSub -> [FunDec] -> Value 

binOperation op e1 e2 lsub decs = NumValue (op n1 n2)  
 where  
  (NumValue n1) = interp e1 lsub decs
  (NumValue n2) = interp e2 lsub decs

inc = FunDec "inc" ["x"] (Add (Ref "x") (Num 1))
inc2 = FunDec "inc2" ["x" , "y", "z"] (Add (Add (Add (Ref "x") (Ref "z"))  (Ref "y")) (Add (Ref "x") (Num 4)))
