{-# OPTIONS -W #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Here we encode the ``target language'', the language
-- to express denotations (or, meanings)
-- We use the same technique to embed a formal language
-- as the one we used before to embed a fraghment of
-- English (cf. Montague's famous quote).

-- To warm-up, we start with the language of
-- classical propositional logic (without equality)
-- Formula simplification (the interpreter P below)
-- is much easier to explain in the propositional case.

module Prop where

import qualified Data.Set as S

-- Embedding of the language of propositional logic

class Prop lrepr where
  -- Propositional letters 
  pretty'  :: lrepr
  quiet'   :: lrepr
  raining' :: lrepr
  -- (more can be added later)

  true     :: lrepr
  neg      :: lrepr -> lrepr
  conj     :: lrepr -> lrepr -> lrepr

-- sample formulas
fml1 = neg (conj (neg (neg true)) (neg true))
fml2 = quiet' `conj` raining'
fml3 = pretty' `conj` neg raining' `conj` true `conj` pretty'

-- Ex: Where is false and disjunction?
-- How to write TRUE AND (FALSE == (TRUE OR P))






-- Abbreviations
-- They are not part of Prop (not part of the language)
-- Consider them as `abbreviations', of `macros'

false    = neg true
disj x y = neg (conj (neg x) (neg y))

fml4 = true `conj` (false `disj` pretty')

-- I still haven't written the exercise. It remains
-- an exercise. We need to add implication and
-- equivalence.



-- ------------------------------------------------------------------------
-- Models

-- First model: the domain is Bool (Boolean algebra with two
-- constants, True and False)

data R = R { unR :: Bool }

instance Prop R where
  -- One particular interpretation function
  -- interpreting prop letters
  pretty'                = R True
  quiet'                 = R True
  raining'               = R False

  -- Logical part of the model
  true                   = R True
  neg (R True)           = R False
  neg (R False)          = R True
  conj (R True) (R True) = R True
  conj _        _        = R False


-- Evaluate our examples in the model

fml1R = unR fml1
fml2R = unR fml2
fml3R = unR fml3
fml4R = unR fml4

-- Exercise (adv): the instance R has the interpretation
-- of propositional letters wired-in.
-- If we wish to try a different interpretation function,
-- we have to re-write the instance, repeating
-- neg and conj (which stay constant)
-- Re-write R so that the interpretation of propositional
-- letters (I) is a parameter, to be specified by the user.




-- We can give another model: the standard set-theoretical
-- one, in a richer Boolean algebra (posets)

-- Ex: What are the deriving Eq and Ord, and why we need them?

data Event = PP | PQ | PR
	deriving (Eq, Ord, Show)


-- Ex: What is the meaning of the following?
s_top = S.fromList [PP,PQ,PR]


data RS = RS{unRS :: S.Set Event}

instance Prop RS where
  -- One particular interpretation function
  -- interpreting prop letters
  pretty'                = RS (S.singleton PP)
  quiet'                 = RS (S.singleton PQ)
  raining'               = RS (S.singleton PR)

  -- Logical part of the model
  true                   = RS s_top
  neg (RS s)             = RS (S.difference s_top s)
  conj (RS s1) (RS s2)   = RS (S.intersection s1 s2)


fml1RS = unRS fml1
fml2RS = unRS fml2
fml3RS = unRS fml3
fml4RS = unRS fml4

-- Compare  fml3RS vs fml3R and the same for fml4
-- See the difference? Why?
-- fml1R and fml3R were the same; but fml1RS and fml3RS are not
-- Some models are more discriminating...
-- Is it possible to build a model that is even more discriminating?



-- And yet another `model'. Why do we need it?
-- Is it a real model?


data C = C { unC :: String }

instance Prop C where
  pretty'          = C "P"
  quiet'           = C "Q"
  raining'         = C "R"
  true             = C "T"
  neg (C x)        = C ("-" ++ x)
  conj (C x) (C y) = C ("(" ++ x ++ " & " ++ y ++ ")")

fml1C = unC fml1
fml2C = unC fml2
fml3C = unC fml3
fml4C = unC fml4


-- ------------------------------------------------------------------------
-- Simplification: (very) simple inference

-- an instance of a language _transformation_

-- data P0 lrepr = P0{unP0 :: lrepr}
data P0 lrepr = P0 lrepr
unP0 (P0 x) = x

instance (Prop lrepr) => Prop (P0 lrepr) where
  -- is there a weird self-reference in here?
  -- how could this possibly work?
  pretty'            = P0 pretty'
  quiet'             = P0 quiet'
  raining'           = P0 raining'
  true               = P0 true
  neg (P0 x)         = P0 (neg x)
  conj (P0 xs) (P0 ys) = P0 (conj xs ys)


-- How to use P0?


fml1P0C = unC . unP0 $ fml1
fml1P0C' = unC (unP0 fml1)



-- Isn't this stupid?
-- What possible use could be for P0?
-- (if it isn't a rhetorical question, it should be)

-- What is the intended meaning for known?
data P lrepr = P { unP :: lrepr, known :: Maybe Bool }

instance (Prop lrepr) => Prop (P lrepr) where
  pretty'            = P pretty' Nothing
  quiet'             = P quiet' Nothing
  raining'           = P raining' Nothing

  true               = P true (Just True)

  neg (P _ (Just False)) = true
  neg (P _ (Just True))  = P (neg true) (Just False)
  -- Why not to put false on the right-hand side of the above?
  neg (P x _)            = P (neg x) Nothing

  conj (P _ (Just True)) y       = y
  conj x (P _ (Just True))       = x
  conj (P _ (Just False)) _      = false
  conj _ (P _ (Just False))      = false
  conj (P x _) (P y _)           = P (conj x y) Nothing


fml1PC = unC . unP $ fml1
fml2PC = unC . unP $ fml2
fml3PC = unC . unP $ fml3
fml4PC = unC . unP $ fml4

-- It is good. Still we can do better.
-- Can we simplify double-negation and repeated
-- occurrences of the same literal?
-- Does this remind you of normalization?
