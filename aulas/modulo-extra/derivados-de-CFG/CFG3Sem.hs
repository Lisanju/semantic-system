{-# LANGUAGE EmptyDataDecls, FlexibleInstances, TypeFamilies #-}

-- Type functions: interpretations of the type constants
-- Compare with CFG2ENDyn.hs: the assurances are the same,
-- but bad derivations are rejected at compile time.
-- Bad derivations cannot even be entered.

module CFG3Sem where

data S					-- clause
data NP					-- noun phrase
data VP					-- verb phrase
data TV					-- transitive verb

-- Since we have syntactic types (categories) now, we have to relate 
-- syntactic categories and semantic types (Entity and Bool)
-- What we do below is establishing syntax-semantic interface.
-- But in a round-about way, since we had to cut-and-paste
-- from CFG3EN. We will solve the cut-and-paste problem in
-- CFG4.hs

type family Tr (a :: *) :: *
type instance Tr S  = Bool
type instance Tr NP = Entity
type instance Tr VP = Entity -> Bool
type instance Tr TV = Entity -> Entity -> Bool

-- data Sem a = Sem { unSem :: Tr a }
data Sem a = Sem (Tr a)
unSem (Sem x) = x

data Entity = John | Mary
  deriving (Eq, Show)

john, mary :: Sem NP
like       :: Sem TV
r2         :: Sem TV -> Sem NP -> Sem VP
r1         :: Sem NP -> Sem VP -> Sem S

john                 = Sem John
mary                 = Sem Mary
like                 = Sem (\o s -> elem (s,o) [(John,Mary), (Mary,John)])

r2 (Sem tv) (Sem np) = Sem (tv np)
r1 (Sem np) (Sem vp) = Sem (vp np)

instance Show (Sem S) where
  show (Sem x) = show x

sentence :: Sem S
sentence = r1 john (r2 like mary)


-- How to tell if the result of evaluating the sentence 
-- shows that John likes Mary or that Mary likes John?
-- We could trace the evaluation.
-- A better idea is to display the denotation as a formula
-- rather than as its value in one particular world.
