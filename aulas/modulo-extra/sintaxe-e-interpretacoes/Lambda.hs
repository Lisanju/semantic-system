{-# OPTIONS -W #-}
{-# LANGUAGE TypeFamilies, NoMonomorphismRestriction #-}

-- Embedding of a higher-order language
-- The main feature is the presence of binding
-- (e.g., logical quantification).

-- We use a pure lambda calculus here, before
-- getting to the predicate logic

module Lambda where

-- First, we define the syntax of lambda-calculus

class Lambda lrepr where
  app :: lrepr (a -> b) -> lrepr a -> lrepr b
  lam :: (lrepr a -> lrepr b) -> lrepr (a -> b)

-- Examples
identity = lam (\x -> x)
twice = lam (\f -> lam (\x -> app f (app f x)))
twicetwice = app twice twice

-- The first interpretation (semantics)
-- Interpret lambda-terms as Haskell terms (functions)

data R a = R { unR :: a }

instance Lambda R where
  app (R f) (R x) = R (f x)
  lam f           = R (\x -> unR (f (R x)))

idR = unR identity "hello"
twR = unR twice (+1) 0
ttR = unR twicetwice (+1) 0

-- The second interpretation: showing lambda-terms
-- We can ``see functions''

-- The alias VarCounter should tell the intended meaning:
-- the counter to make variable names.
type VarCounter = Int

data C a = C { unC :: VarCounter -> String }

instance Lambda C where
  app (C f) (C x) = C (\i -> "(" ++ f i ++ " " ++ x i ++ ")")
  lam f           = C (\i -> let x    = "x" ++ show i
                                 body = unC (f (C (\_ -> x))) (i + 1)
                             in "L" ++ x ++ ". " ++ body)
showL (C x) = x 0

idC = showL identity
twC = showL twice
ttC = showL twicetwice

-- We can see abstractions!
-- Some terms are big (twicetwice) and have obvious
-- beta-redices. Can we simplify them before displaying?

type family Known (lrepr :: * -> *) (a :: *)
type instance Known lrepr (a -> b) = P lrepr a -> P lrepr b
data P lrepr a = P { unP :: lrepr a, known :: Maybe (Known lrepr a) }

instance (Lambda lrepr) => Lambda (P lrepr) where
  app (P _ (Just f)) x      = f x
  app (P f Nothing) (P x _) = P (app f x) Nothing
  lam f                     = P (lam (\x -> unP (f (P x Nothing)))) (Just f)


idP = showL . unP $ identity
twP = showL . unP $ twice
ttP = showL . unP $ twicetwice

-- ttP looks much better than ttC
-- Haskell is indeed lambda-calculuator.
-- Not only it does reductions, it also shows the result.
