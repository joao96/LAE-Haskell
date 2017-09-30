module F3LAE where 

import Prelude hiding (lookup)

type Name = String  
type FormalArg = String 
type Id = String 

data FunDec = FunDec Name FormalArg Exp 

data Exp = Num Integer
         | Add Exp Exp 
         | Sub Exp Exp 
         | Ref Id
         | Let Id Exp Exp 
         | App Name Exp 
         | Lambda FormalArg Exp
         | AppLambda Exp Exp
     deriving(Show, Eq)     

type DefrdSub = [(Id, Value)] 

data Value = NumValue Integer
           | Closure FormalArg Exp DefrdSub  
    deriving(Show, Eq)

interp :: Exp -> DefrdSub -> [FunDec] -> Value

interp (Num n)  _ _                = NumValue n
interp (Add l r) lsub decs         = binOperation (+) l r lsub decs 
interp (Sub l r) lsub decs         = binOperation (-) l r lsub decs


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

interp (App name expNom) lsub decs =
  let f = lookup name decs
   in case f of
    (Nothing) -> error "Function not declared"
    (Just (FunDec n fArg corpo)) -> interp corpo ((fArg, interp expNom lsub decs) : lsub) decs  

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

inc = FunDec "inc" "x" (Add (Ref "x") (Num 1))


test1 = TestCase (assertEqual "for (foo 3)," (1,2) (foo 3))