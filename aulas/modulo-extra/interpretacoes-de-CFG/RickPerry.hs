{-# LANGUAGE EmptyDataDecls, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

-- Representing the derivation of, and computing the yield 
-- and truth values of two sample sentences from the Semantics boot camp:
--    Rick Perry is conservative
--    Rick Perry is in Texas
--
-- Elizabeth Coppock. Semantics bootcamp handouts (Part III,
-- \S1.1 and \S1.2) NASSLLI 2012, June 16, 2012
-- http://nasslli2012.com/bootcamp

module RickPerry where

-- Re-using the existing framework
import CFG
import Semantics

-- We need two more categories, adjectives and copula
-- (we use the same names as in the handout)
data A
data V

-- We add a new class, `extending', so to speak, Symantics.
-- How to read the above declaration:
-- Any RickPerry interpretation must be Symantics
-- (base language) interpretation, and in addition,
-- interpret the following constants and rules.

class Symantics repr => RickPerry repr where
    rick_perry   :: repr NP
    conservative :: repr A
    is           :: repr V

    -- new grammar rule
    ra :: repr V -> repr A -> repr VP

-- The type is inferred
-- sen :: RickPerry repr => repr S
sen = r1 rick_perry (ra is conservative)

instance RickPerry EN where
    rick_perry   = EN "Rick Perry"
    conservative = EN "conservative"
    is           = EN "is"
    ra (EN v) (EN a) = EN (v ++ " " ++ a)

senC = sen :: EN S

-- Add to the semantics

-- New domain constants for RickPerry and conservative,
-- with the semantic types given in the handout
class Lambda lrepr => LRickPerry lrepr where
    rick_perry'   :: lrepr Entity
    conservative' :: lrepr (Entity -> Bool)

instance LRickPerry C where
  rick_perry'   = C (\_ _ -> "RickPerry'")
  conservative' = C (\_ _ -> "conservative'")

instance (LRickPerry lrepr) => LRickPerry (P lrepr) where
  rick_perry'     = unknown rick_perry'
  conservative'   = unknown conservative'

-- We add the interpretation of the categories A and V
-- matching the semantic types in the handout
type instance Tr V  = (Entity -> Bool) -> (Entity -> Bool)
type instance Tr A  = (Entity -> Bool)

instance (LRickPerry lrepr) => RickPerry (Sem lrepr) where
  rick_perry   = Sem rick_perry'
  conservative = Sem conservative'
  is           = Sem (lam (\x -> x))
  ra (Sem v) (Sem a) = Sem (app v a)


senS  = sen :: Sem C S
senS' = sen :: Sem (P C) S

-- We see the logical formula, which we can then evaluate in
-- a particular model, with a particular interpretation
-- of `conservative'

-- Adjectives (conservative) has the same semantic type as VP.
-- However, adjectives are not substitutable for VP. 
-- We can't say
--   Rick Perry conservative
-- That is why we need syntactic categories in addition to
-- semantic types.
