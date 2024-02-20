module CFG2EN where

-- Type annotations

john, mary :: String
like       :: String
r2         :: String -> String -> String
r1         :: String -> String -> String

john   = "John"
mary   = "Mary"
like   = "likes"
r2 tv np = tv ++ " " ++ np
r1 np vp = np ++ " " ++ vp

sentence :: String
sentence = r1 john (r2 like mary)

-- Unfortunately, the following sentence is, too,
-- accepted by the type checker.
bad_sentence :: String
bad_sentence = r2 (r2 like mary) john

-- and even the total nonsense ...
bad_sentence' :: String
bad_sentence' = r2 (r2 john john) john

-- We shall later see how to build terms that correspond to
-- all and only valid derivations.
-- Invalid derivations will become ill-typed.
