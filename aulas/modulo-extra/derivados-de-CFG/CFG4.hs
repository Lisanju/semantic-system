{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-- Unifying syntax with semantics

module CFG4 where

data S					-- clause
data NP					-- noun phrase
data VP					-- verb phrase
data TV					-- transitive verb

class Symantics repr where
  john, mary :: repr NP
  like       :: repr TV
  r2         :: repr TV -> repr NP -> repr VP
  r1         :: repr NP -> repr VP -> repr S

sentence = r1 john (r2 like mary)

data EN a = EN { unEN :: String }

instance Symantics EN where
  john             = EN "John"
  mary             = EN "Mary"
  like             = EN "likes"
  r2 (EN f) (EN x) = EN (f ++ " " ++ x)
  r1 (EN x) (EN f) = EN (x ++ " " ++ f)

instance Show (EN a) where
  show = unEN

sentence_en = sentence :: EN S

type family Tr (a :: *) :: *
type instance Tr S  = Bool
type instance Tr NP = Entity
type instance Tr VP = Entity -> Bool
type instance Tr TV = Entity -> Entity -> Bool

data Sem a = Sem { unSem :: Tr a }

data Entity = John | Mary
  deriving (Eq, Show)

instance Symantics Sem where
  john               = Sem John
  mary               = Sem Mary
  like               = Sem (\o s -> elem (s,o) [(John,Mary), (Mary,John)])
  r2 (Sem f) (Sem x) = Sem (f x)
  r1 (Sem x) (Sem f) = Sem (f x)

instance Show (Sem S) where
  show (Sem x) = show x

sentence_sem = sentence :: Sem S

data Graphviz a = Graphviz { unGraphviz :: String -> String }

render_tree :: String -> [String -> String] -> String -> String
render_tree label children node =
  let child g i = g (node ++ "_" ++ show i)
      edge  g i = node ++ " -> " ++ node ++ "_" ++ show i ++ ";\n"
  in concat (zipWith child children [1..]) ++
     node ++ " [label=\"" ++ label ++ "\"];\n" ++
     concat (zipWith edge children [1..])

instance Symantics Graphviz where
  john                           = Graphviz (render_tree "john" [])
  mary                           = Graphviz (render_tree "mary" [])
  like                           = Graphviz (render_tree "like" [])
  r2 (Graphviz g1) (Graphviz g2) = Graphviz (render_tree "r2" [g1,g2])
  r1 (Graphviz g1) (Graphviz g2) = Graphviz (render_tree "r1" [g1,g2])

instance Show (Graphviz a) where
  show (Graphviz g) = "strict digraph derivation {\n" ++
                      "node [shape=plaintext,fontsize=20];\n" ++
                      "edge [dir=none,sametail];\n" ++
                      g "n" ++
                      "}"

sentence_graphviz = sentence :: Graphviz S

main = print sentence_graphviz
