module Logic 

where 
import List
import Char
import FormContent

data Form = Prop String
          | Not Form
          | Conj [Form]
          | Disj [Form]
          deriving (Eq,Ord) 

p1 = Prop "p1" ; p2 = Prop "p2" ; p3 = Prop "p3"
q1 = Prop "q1" ; q2 = Prop "q2" ; q3 = Prop "q3"
r1 = Prop "r1" ; r2 = Prop "r2" ; r3 = Prop "r3"

frm1 = Disj [p1, Not p1]
frm2 = Conj [p1, Not p1]

instance Show Form where
  show (Prop name) = name
  show (Not f) = '~' : '(' : show f ++ ")"
  show (Conj fs) = '&' : show fs
  show (Disj fs) = 'v' : show fs

collectVars :: Form -> [String]
collectVars (Prop name) = [name]
collectVars (Not f) = collectVars f
collectVars (Conj fs) = 
  (nub.concat) (map collectVars fs) 
collectVars (Disj fs) = 
  (nub.concat) (map collectVars fs) 

allVals :: Form -> [[(String,Bool)]]
allVals f = cv (collectVars f)
  where 
   cv [] = [[]]
   cv (v:vs) = map ((v,True):) (cv vs)
               ++ 
               map ((v,False):) (cv vs) 

eval :: [(String,Bool)] -> Form -> Bool
eval [] (Prop c) = error ("no info about " ++ c)
eval ((x,b):xs) (Prop c) 
      | c == x    = b 
      | otherwise = eval xs (Prop c) 
eval xs (Not f)   = not (eval xs f)
eval xs (Conj fs) = all (eval xs) fs
eval xs (Disj fs) = any (eval xs) fs

tautology :: Form -> Bool
tautology f = 
   all (\ v -> eval v f) (allVals f)

contradiction :: Form -> Bool
contradiction f = 
   not (any  (\ v -> eval v f) (allVals f))

satisfiable :: Form -> Bool
satisfiable  f = 
   any (\ v -> eval v f) (allVals f)

implies :: Form -> Form -> Bool
implies f1 f2 = contradiction (Conj [f1, Not f2])

type Name = String
data Var  = Var Name deriving (Eq,Ord)

instance Show Var where 
  show (Var name)  = name

x, y, z :: Var 
x  = Var "x" 
y  = Var "y" 
z  = Var "z" 

data Frm = Pred String [Var]
         | Eql Var Var 
         | Neg Frm
         | Impl Frm Frm
         | Equiv Frm Frm
         | Cnj [Frm]
         | Dsj [Frm] 
         | Forall Var Frm 
         | Exists Var Frm
     deriving (Eq,Ord)

instance Show Frm where 
  show (Pred str []) = str
  show (Pred str vs) = str ++ concat [ show vs ]
  show (Eql v1 v2)   = show v1 ++ "==" ++ show v2
  show (Neg form)   = '~': (show form)
  show (Impl f1 f2) = 
     "(" ++ show f1 ++ "==>" ++ show f2 ++ ")"
  show (Equiv f1 f2) = 
     "(" ++ show f1 ++ "<=>" ++ show f2 ++ ")"
  show (Cnj []) =  "true" 
  show (Cnj fs) =  "conj" ++ concat [ show fs ]
  show (Dsj []) =  "false" 
  show (Dsj fs) =  "disj" ++ concat [ show fs ]

  show (Forall v f) = 
     "A" ++  show v ++ (' ' : show f)
  show (Exists v f) = 
     "E" ++  show v ++ (' ' : show f)

form0 = Pred "R" [x,y]
form1 = Pred "R" [x,x]
form2 = Exists x form1
reflF = Forall x form1
symmF = Forall x 
         (Forall y
           (Impl (Pred "R" [x,y]) 
                 (Pred "R" [y,x])))
transF = Forall x 
         (Forall y
           (Forall z
             (Impl (Cnj [Pred "R" [x,y],
                         Pred "R" [y,z]]) 
                        (Pred "R" [x,z]))))

change :: (Var -> a) -> Var -> a -> Var -> a 
change s x d = \ v -> if x == v then d else s v

ass0 :: Var -> Entity
ass0 = \ v -> A

ass1 :: Var -> Entity
ass1 = change ass0 y B

evl :: Eq a => 
 [a]                         -- domain of discourse 
  -> (String -> [a] -> Bool) -- interpretation
  -> (Var -> a)              -- valuation 
  -> Frm                     -- formula 
  -> Bool                    -- outcome 

evl dom i s (Pred str vs) = i str (map s vs)
evl dom i s (Eql v1 v2)   = (s v1) == (s v2) 
evl dom i s (Neg f)       = not (evl dom i s f)
evl dom i s (Impl f1 f2)  = 
   not ((evl dom i s f1) && not (evl dom i s f2))
evl dom i s (Equiv f1 f2)  = 
   (evl dom i s f1) == (evl dom i s f2)
evl dom i s (Cnj fs)     = all (evl dom i s) fs
evl dom i s (Dsj fs)     = any (evl dom i s) fs
evl dom i s (Forall v f)  = 
   all (\d -> evl dom i (change s v d) f) dom
evl dom i s (Exists v f)  = 
   any (\d -> evl dom i (change s v d) f) dom

int0 :: String -> [Entity] -> Bool
int0 "P" = \ [x] -> laugh x
int0 "Q" = \ [x] -> smile x
int0 "R" = \ [x,y] -> love (x,y)
int0 "S" = \ [x,y] -> hate (x,y)

int1 :: String -> [Integer] -> Bool
int1 "P" = \ [x] -> even x
int1 "Q" = \ [x] -> x > 0
int1 "R" = \ [x,y] -> x < y 
int1 "S" = \ [x,y] -> x <= y

ass2 :: Var -> Integer
ass2 v = if v == x then 1 
         else if v == y then 2 else 0
