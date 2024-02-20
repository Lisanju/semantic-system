module CFG2Sem where

-- CFG1Sem with type annotations

data Entity = John | Mary
  deriving (Eq, Show)

john, mary :: Entity
like       :: Entity -> Entity -> Bool
r2         :: (Entity -> Entity -> Bool) -> Entity -> (Entity -> Bool)
r1         :: Entity -> (Entity -> Bool) -> Bool

john   = John
mary   = Mary

-- A new notation for `like' (which will be convenient later)
like   = \o s -> elem (s,o) [(John,Mary), (Mary,John)]

r2 tv np = tv np
r1 np vp = vp np

sentence :: Bool
sentence = r1 john (r2 like mary)

-- In the Sem interpretation, the bad_sentence
-- is ill-typed, as it should. Note the error message
-- So, we use Semantics to fix the problem with bad
-- (too permissive) grammar embedding. We use semantics
-- to fix syntax! (cf. `Linking' part of the semantics
-- bootcamp).

{-
bad_sentence :: Bool
bad_sentence = r2 (r2 like mary) john
-}

-- Same here
{-
bad_sentence' :: Bool
bad_sentence' = r2 (r2 john john) john
-}
