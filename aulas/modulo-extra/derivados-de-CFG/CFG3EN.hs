{-# LANGUAGE EmptyDataDecls #-}

-- Introducing type constants

-- We wish to outlaw terms such as bad_sentence in CFG2EN.hs,
-- even though there may be an interpretation that accepts
-- these bad terms.
-- We really wish our terms represent all and only
-- valid CFG derivations. We accomplish this goal here.
-- Our approach is reminiscent of LCF.

module CFG3EN where

-- The declarations below define types but not constructors
-- (because we never build the values). These are just labels.
-- Compare with CFG2ENDyn.hs
data S					-- clause
data NP					-- noun phrase
data VP					-- verb phrase
data TV					-- transitive verb

-- Parameterized types: cf notation <string,features> in
-- the Minimalist Grammar

data EN a = EN String
unEN (EN x) = x
-- The two lines above can be written more concisely as
-- follows:
-- data EN a = EN { unEN :: String }

-- One may think of the above data declaration as defining an
-- isomorphism between EN values and Strings. The functions
-- EN and unEN (what is their type?) witness the isomorphism.
-- It helps to look at their composition.


-- Now, the signatures below are necessary!
-- They cannot be inferred.
-- They look quite like the specification of the grammar.
john, mary :: EN NP
like       :: EN TV
r2         :: EN TV -> EN NP -> EN VP
r1         :: EN NP -> EN VP -> EN S

-- If we want avoid giving the signature, we could
-- assign the explicit type to john as below.
john               = EN "John" :: EN NP
mary               = EN "Mary"
like               = EN "likes"
r2 (EN tv) (EN np) = EN (tv ++ " " ++ np)
r1 (EN np) (EN vp) = EN (np ++ " " ++ vp)

-- In CFG2EN, r1 and r2 were the same functions.
-- In CFG2EnDyn, r1 and r2 had the same type, but were
-- different functions (already extensionally)
-- Now, r1 and r2 have different types!

instance Show (EN a) where
  show = unEN

sentence :: EN S
sentence = r1 john (r2 like mary)

-- Now the bad_sentence is rejected already in
-- the EN interpretation, in contrast to CFG2EN.hs.
-- The type error message clearly describes the error,
-- in the CFG terms.
-- bad_sentence = r2 (r2 like mary) john

-- Same here
-- bad_sentence' = r2 (r2 john john) john

-- Moreover, even a bad constituent already causes an error
-- (in constrast to the example in CFG2ENDyn.hs)
-- jj = (r2 john john)
