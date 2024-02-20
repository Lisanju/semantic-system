module FormContent

where 
import List
import Char

data Sent = Sent NP VP 
     deriving (Eq,Show)
 
data NP = Ann | Mary | Bill | Johnny 
        | NP1 DET CN | NP2 DET RCN
     deriving (Eq,Show)

data DET = Every | Some | No | The | Most 
         | Atleast Int
     deriving (Eq,Show)

data CN = Man | Woman | Boy | Person 
        | Thing | House
     deriving (Eq,Show)

data RCN = CN1 CN VP | CN2 CN NP TV 
     deriving (Eq,Show)

data VP = Laughed | Smiled | VP1 TV NP 
     deriving (Eq,Show)

data TV = Loved | Respected | Hated | Owned
     deriving (Eq,Show)

data Entity = A | B | C | D | E | F | G
            | H | I | J | K | L | M | N 
            | O | P | Q | R | S | T | U 
            | V | W | X | Y | Z | Unspec
     deriving (Eq,Bounded,Enum)

entities :: [Entity]
entities = [minBound..maxBound] 

instance Show Entity where 
  show (A) = "A"; show (B) = "B"; show (C) = "C"; 
  show (D) = "D"; show (E) = "E"; show (F) = "F"; 
  show (G) = "G"; show (H) = "H"; show (I) = "I"; 
  show (J) = "J"; show (K) = "K"; show (L) = "L";  
  show (M) = "M"; show (N) = "N"; show (O) = "O"; 
  show (P) = "P"; show (Q) = "Q"; show (R) = "R"; 
  show (S) = "S"; show (T) = "T"; show (U) = "U"; 
  show (V) = "V"; show (W) = "W"; show (X) = "X"; 
  show (Y) = "Y"; show (Z) = "Z"; show (Unspec)= "*"

rel1 :: Entity -> Entity -> Bool
rel1 A A = True
rel1 B A = True
rel1 D A = True
rel1 C B = True
rel1 C C = True 
rel1 C D = True 
rel1 _ _ = False 

self :: (a -> a -> b) -> a -> b
self = \ f x -> f x x 

rel2 = self rel1

ann, bill, lucy, mary, johnny :: Entity 
ann    = A; bill   = B; lucy = L
mary   = M; johnny = J 

list2pred :: Eq a => [a] -> a -> Bool 
list2pred = flip elem

man, boy, woman, tree, house :: Entity -> Bool
leaf, stone, gun, person, thing :: Entity -> Bool
man    = list2pred [B,J]
woman  = list2pred [A,C,M,L]
boy    = list2pred [J]
tree   = list2pred [T,U,V]
house  = list2pred [H,K]
leaf   = list2pred [X,Y,Z]
stone  = list2pred [S]
gun    = list2pred [G]

person = \ x -> (man x || woman x)
thing  = \ x -> not (person x || x == Unspec)

laugh, smile :: Entity -> Bool
laugh = list2pred [M]
smile = list2pred [A,B,J,M]

love, respect, hate, own, wash, shave, drop0 
          :: (Entity, Entity) -> Bool
love    = list2pred 
          [(B,M),(J,M),(J,J),(M,J),(A,J),(B,J)]
respect = list2pred [(x,x) 
                        | x <- entities, person x ]
hate    = list2pred [(x,B)
                         | x <- entities, woman x ]
own     = list2pred [(M,H)]
wash    = list2pred [(A,A),(A,J),(L,L),(B,B),(M,M)]
shave   = list2pred [(A,J),(B,B)]
drop0   = list2pred [(T,X),(U,Y),(U,Z),(Unspec,V)]

break0, kill :: 
         (Entity, Entity, Entity) -> Bool
break0 = list2pred [(M,V,S), (J,W,G)]
kill   = list2pred 
         [(M,L,G), (Unspec,A,D), (Unspec,J,Unspec)]

give, sell :: (Entity, Entity, Entity) -> Bool
give = list2pred [(M,V,L), (L,G,M)]
sell = list2pred [(J,J,M), (J,T,M), (A,U,M)]

curry3 :: ((a,b,c) -> d) 
           -> a -> b -> c -> d
curry3 f x y z = f (x,y,z)

uncurry3     :: 
  (a -> b -> c -> d) -> ((a,b,c) -> d)
uncurry3 f (x,y,z)  = f x y z

intSent :: Sent -> Bool 
intSent (Sent np vp) = (intNP np) (intVP vp)

intNP :: NP -> (Entity -> Bool) -> Bool
intNP Ann = \ p -> p ann 
intNP Mary = \ p -> p mary
intNP Bill = \ p -> p bill
intNP Johnny = \ p -> p johnny
intNP (NP1 det cn) = (intDET det) (intCN cn) 
intNP (NP2 det rcn) = (intDET det) (intRCN rcn) 

intVP :: VP -> Entity -> Bool 
intVP Laughed = laugh
intVP Smiled = smile 

intVP (VP1 tv np) = 
   \ subj -> 
       intNP np (\ obj -> intTV tv (subj,obj))

intTV :: TV -> (Entity,Entity) -> Bool
intTV Loved     = love
intTV Respected = respect
intTV Hated     = hate
intTV Owned     = own

intCN :: CN -> Entity -> Bool
intCN Man = man 
intCN Boy = boy
intCN Woman = woman 
intCN Person = person 
intCN Thing = thing 
intCN House = house

intDET :: DET -> (Entity -> Bool) 
          -> (Entity -> Bool) -> Bool

intDET Some p q = any q (filter p entities)

intDET Every p q = all q (filter p entities)

intDET The p q = singleton plist && q (head plist) 
          where 
              plist = filter p entities
              singleton [x] = True 
              singleton _   = False

intDET No p q = not (intDET Some p q) 

intDET Most p q = length pqlist > 
                  length (plist \\ qlist)
  where 
  plist  = filter p entities 
  qlist  = filter q entities 
  pqlist = filter q plist

intRCN :: RCN -> Entity -> Bool
intRCN (CN1 cn vp) = 
        \ e -> ((intCN cn e) && (intVP vp e))

intRCN (CN2 cn np tv) = \ e -> 
    ((intCN cn e) && 
     (intNP np (\ subj -> (intTV tv (subj,e)))))
