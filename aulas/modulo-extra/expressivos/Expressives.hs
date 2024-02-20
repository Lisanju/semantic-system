{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

-- Analysis of expressives:
--    John liked the damn donkey.
--    Damn, John liked the damn donkey.

-- This file uses a tagless-final framework
-- illustrated in the  NASSLLI 2012 / ESSLLI 2013 course. 
--   http://okmij.org/ftp/gengo/NASSLLI10/
-- In particular, see the file CFG4.hs

module Expressives where

import Semantics
import Control.Applicative

-- Syntactic categories
data S					-- clause
data NP					-- noun phrase
data VP					-- verb phrase
data TV					-- transitive verb
data ADJ				-- adjectives
data ADV 				-- sentence adverbs
data CN					-- common noun

-- A fragment of a language

-- The class defines the CFG grammar for the language
class Symantics repr where
  -- terminals, marked with their categories
  john, mary :: repr NP
  like       :: repr TV
  donkey     :: repr CN
  green      :: repr ADJ

  -- combining rules, or: the rules of the CFG grammar
  r2         :: repr TV  -> repr NP -> repr VP
  r1         :: repr NP  -> repr VP -> repr S
  the        :: repr CN  -> repr NP
  -- adjective complementation
  radj       :: repr ADJ -> repr CN -> repr CN
  -- adverb complementation, of a sentence
  radv       :: repr ADV -> repr S -> repr S

-- Extending the grammar with damnations
class Symantics repr => Upset repr where
  damn_adj   :: repr ADJ
  damn_adv   :: repr ADV

-- Sample sentences
-- (The types ensure that sentences are well-formed according to the
-- grammar)
-- The expressions below are derivations (trees); read `r1', `r2', etc.
-- as internal nodes.
-- One can also read out the sentence from the derivation, by
-- pronouncing terminals.

sen1  = john `r1` (like `r2` the donkey)

sena  = john `r1` (like `r2` the (green `radj` donkey))

send  = john `r1` (like `r2` the (damn_adj `radj` donkey))

senad = john `r1` (like `r2` the (damn_adj `radj` (green `radj` donkey)))

sendd = damn_adv `radv` (john `r1`  (like `r2` the (damn_adj `radj` donkey)))


-- Displaying the yield of the grammar
data EN a = EN { unEN :: String }

instance Symantics EN where
  john             = EN "John"
  mary             = EN "Mary"
  like             = EN "liked"
  donkey           = EN "donkey"
  green            = EN "green"

  r2 (EN f) (EN x)   = EN (f ++ " " ++ x)
  r1 (EN x) (EN f)   = EN (x ++ " " ++ f)
  radj (EN f) (EN x) = EN (f ++ " " ++ x)
  radv (EN f) (EN x) = EN (f ++ " " ++ x)
  the (EN x)         = EN ("the " ++ x) 

instance Upset EN where
  damn_adj         = EN "damn"
  damn_adv         = EN "damn"

instance Show (EN a) where
  show = unEN

-- Yields for the sample sentences
sen1_en = sen1 :: EN S
-- John liked the donkey

sena_en  = sena :: EN S
-- John liked the green donkey

send_en  = send :: EN S
-- John liked the damn donkey

senad_en = senad :: EN S
-- John liked the damn green donkey

sendd_en = sendd :: EN S
-- damn John liked the damn donkey


-- Syntax-Semantic interface
-- Relating Grammar trees with Logic formulas

-- Mapping syntactic categories to semantic types
type family Tr (a :: *) :: *
type instance Tr S   = Bool
type instance Tr NP  = Entity
type instance Tr CN  = Entity -> Bool
type instance Tr VP  = Entity -> Bool
type instance Tr TV  = Entity -> Entity -> Bool
-- We consider intersecting adjectives (such as adjectives of color,
-- nationality, gender, etc): 
-- their interpretation is independent of the interpretation of the
-- noun they modify.
type instance Tr ADJ = Entity -> Bool
-- Sentence adverbs affect the proposition expressed by the sentence.
-- In our very simple semantics, the proposition is a Bool value,
-- hence sentence adverbs can do little with it except to 
-- possibly negate it. (That can happen, see the sentence adverb ``Falsely'')
-- If a proposition has more structure, then the semantics
-- of sentence adverbs as proposition modifiers becomes richer.
type instance Tr ADV = Bool -> Bool

-- Building a logical formula in an applicative i
data Sem i lrepr a = Sem { unSem :: i (lrepr (Tr a)) }

-- The following mapping of syntax to semantics works in _any_
-- applicative
instance (Applicative i, Lambda lrepr) => 
         Symantics (Sem i lrepr) where
  john   = Sem (pure john')
  mary   = Sem (pure mary')
  like   = Sem (pure like')
  donkey = Sem (pure donkey')
  green  = Sem (pure green')

  r2 (Sem tv) (Sem np) = Sem (app <$> tv <*> np)
  r1 (Sem np) (Sem vp) = Sem (app <$> vp <*> np)
  the (Sem cn) = Sem ((app iota) <$> cn)
  -- Recall, our adjectives are intersecting
  radj (Sem adj) (Sem cn) = 
      Sem ((\f g -> lam (\x -> app f x `conj` app g x)) <$> adj <*> cn)
  -- A sentence adverb modifies the proposition expressed by the sentence
  radv (Sem adv) (Sem s) = Sem (app <$> adv <*> s)

-- The identity Applicative
newtype IdA x = IdA{unIdA :: x}
    deriving (Functor, Show)
instance Applicative IdA where
    pure = IdA
    IdA f <*> IdA x = IdA (f x)

-- Meaning for the sample sentences
sen1_sem = unSem sen1 :: IdA (P C Bool)
-- IdA {unIdA = (like' (iota donkey')) john'}

sena_sem  = unSem sena :: IdA (P C Bool)
-- IdA {unIdA = like' (I (Lx1. green' x1 & donkey' x1)) john'}

-- Semantics with an expressive
-- It carries the logical formula for the meaning at issue,
-- plus the collection of expressives

data Expressive = Bad
	      deriving Show

-- An Applicative with an Expressive
-- Expressives simply accumulate: a Writer Applicative
data ExpDA x = ExpDA {at_issue :: x,
		      expressive :: [Expressive] }
  deriving Show

instance Functor ExpDA where
    fmap f atta = atta{at_issue = f (at_issue atta)}
instance Applicative ExpDA where
    pure x = ExpDA{at_issue = x, expressive = []}
    ExpDA ix ax <*> ExpDA iy ay = ExpDA (ix iy) (ax ++ ay)
    

-- damnation requires the logic with expressives
instance Lambda lrepr => Upset (Sem ExpDA lrepr) where
  damn_adj = Sem (ExpDA (lam (\x -> true)) [Bad])
  damn_adv = Sem (ExpDA (lam (\x -> x)) [Bad])

send_sem  = unSem send :: ExpDA (P C Bool)
-- ExpDA {at_issue   = like' (I (Lx1. donkey' x1)) john', 
--        expressive = [Bad]}
-- John liked the damn donkey

-- The at-issue content is the same as of the sentence 
-- ``John liked the green donkey''
senad_sem = unSem senad :: ExpDA (P C Bool)
-- ExpDA {at_issue   = like' (I (Lx1. green' x1 & donkey' x1)) john', 
--        expressive = [Bad]}
-- John liked the damn green donkey

sendd_sem = unSem sendd :: ExpDA (P C Bool)
-- ExpDA {at_issue   = like' (I (Lx1. donkey' x1)) john', 
--        expressive = [Bad,Bad]}
-- John damn liked the damn donkey

-- Several aspects of the design determine that the issue
-- content can't affect expressives.
-- First, normal lexical entries like john are defined
-- for any applicative. The polymorphism ensures they can't
-- affect expressives, which are a detail of a specific
-- applicative.
-- Second, since we use applicatives, the content at issue
-- (the `value' produced by an applicative) can't affect
-- the expressives.
-- The expressives are affected only by lexical entries or
-- by some structural combinators (modes) (which we don't
-- have yet).
