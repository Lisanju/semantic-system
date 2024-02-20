module CFG1Sem where

-- Semantic interpretation of a CFG derivation

-- In conventional notation: D_e = {John, Mary}
-- User-defined data types (compare with Either)
-- A data type declaration introduces:
--   -- new type
--   -- introduction rule (constructors)
--   -- elimination rule  (pattern-matching)

data Entity = John | Mary
  deriving (Eq, Show)

john   = John
mary   = Mary

like o s =  (o == John && s == Mary ) || 
	    (o == Mary && s == John )

-- A different way of writing it: by cases
-- The elimination rule
like' Mary John = True
like' John Mary = True
like' _    _    = False

r2 tv np = tv np
r1 np vp = vp np

-- sentence has the same form as in CFG1En.hs,
-- but a different value (interpretation)

sentence = r1 john (r2 like mary)
