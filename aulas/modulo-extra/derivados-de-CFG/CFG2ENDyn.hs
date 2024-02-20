module CFG2ENDyn where

-- Using strings to embed languages, although simple,
-- has many drawbacks. The embedding contains lots of
-- junk.

-- Enforcing the grammar of the object language, at run-time.

-- Terminals and non-terminals are no longer plain strings,
-- but strings paired with a grammatical category:
-- essentially disjoint (`tagged') unions
-- Compare with the notation <string,features> in
-- the Minimalist Grammar

-- The general syntax for data declaration: sums of products
data Repr = 
    S String				-- clause
    | NP String				-- noun phrase
    | VP String				-- verb phrase
    | TV String				-- transitive verb
 deriving Show

john, mary :: Repr
john   = NP "John"
mary   = NP "Mary"

like       :: Repr
like   = TV "likes"

-- Before: r2         :: String -> String -> String
r2         :: Repr -> Repr -> Repr
r2 (TV tv) (NP np) = VP (tv ++ " " ++ np)

-- Before: r1         :: String -> String -> String
r1         :: Repr -> Repr -> Repr
r1 (NP np) (VP vp) = S (np ++ " " ++ vp)

-- In CFG2En, r1 and r2 were the same functions. They are
-- different now.

sentence :: Repr
sentence = r1 john (r2 like mary)

-- Unfortunately, the following sentence is, too,
-- accepted by the type checker.
-- However, an attempt to display the yield gives a runtime error
bad_sentence :: Repr
bad_sentence = r2 (r2 like mary) john


-- The total nonsense is still possible to enter, but
-- no longer possible to show.
bad_sentence' :: Repr
bad_sentence' = r2 (r2 john john) john

-- The drawback of the approach is that we can write a bad
-- constutuent derivation
jj = (r2 john john)

-- and use it as part of other sentences. The error will be
-- reported only when we try to obtain the yield of
-- those other sentences. It would help to get an error report
-- as soon as we attempt to write an invalid constituent derivation
-- like jj.

-- We shall later see how to build terms that correspond to
-- all and only valid derivations.
-- Invalid derivations will become ill-typed.

-- Exercises on sums of products
