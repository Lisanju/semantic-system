{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE TypeOperators, EmptyDataDecls, NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies, Rank2Types #-}

module Tower where

import Semantics

data S
data DP
data N
data result :/ argument
data argument :\ result
data answer :// argument
data argument :\\ answer
data binder :> answer

type Tower c d a = c :// (a :\\ d)

class Symantics repr where
  john     :: repr DP
  mary     :: repr DP
  like     :: repr ((DP :\ S) :/ DP)
  own      :: repr ((DP :\ S) :/ DP)
  farmer   :: repr N
  donkey   :: repr N
  everyone :: repr (Tower S S DP)
  every    :: repr (Tower S S (Tower S S DP :/ N))
  someone  :: repr (Tower S S DP)
  a        :: repr (Tower S S DP :/ N)
  who      :: repr ((N :\ N) :/ (DP :\ S))
  it_      :: repr (Tower (DP :> b) b DP)

-- Shan and Barker (Linguistics and Philosophy 2006) use forward function
-- application and a bunch of type shifters: Lift, Up, Down, Scope, Bind.

  apply_ :: repr (result :/ argument) -> repr argument -> repr result
                                            -- forward function application
  lift_  :: repr (b :/ (a :\ b) :/ a)                               -- Lift
  lift   :: repr (Tower b b a :/ a)                                 -- Up
  down_  :: repr (a :/ Tower a S S)                                 -- Down
  scope_ :: repr (Tower e c b :/ Tower d c a :/ Tower e d (b :/ a)) -- Scope
  bind__ :: repr (Tower b (DP :> a) DP :/ Tower b a DP)             -- Bind

-- Barker and Shan (Semantics and Pragmatics 2008) allow forward function
-- application, backward function application, and the type shifters Lift
-- (renamed from Up), Lower (renamed from Down), and Bind to operate at any
-- level of the tower.  We express these semantic rules in Haskell using
--   1. the binary function (&), which infers direction and level;
--   2. the unary functions (lift #), (skip lift #), (skip (skip lift) #), etc.;
--   3. the unary functions lower and bind, which infer level.

data T f c d a = T { unT :: f (Tower c d a) }
type FMap repr f = forall a b. repr (b :/ a) -> f a -> f b

fmapTower :: (Symantics repr) => FMap repr f -> FMap repr (T f c d)
fmapTower fmap f = T . fmap (skip f) . unT

type family Ap (in1 :: *) (in2 :: *) :: *
type instance Ap (result :/ argument) argument = result
type instance Ap argument (argument :\ result) = result
type instance Ap (Tower c d in1) (Tower d e in2) = Tower c e (Ap in1 in2)

class Apply in1 in2 where
  apply :: (Symantics repr) => FMap repr f -> f in1 -> f (Ap in1 in2 :/ in2)

instance Apply (result :/ argument) argument where
  apply fmap x = x

instance Apply argument (argument :\ result) where
  apply fmap x = fmap lift_ x

instance (d ~ d', Apply in1 in2) => Apply (Tower c d in1) (Tower d' e in2) where
  apply fmap x = fmap scope_ (unT (apply (fmapTower fmap) (T x)))

(&) :: (Symantics repr, Apply in1 in2)
    => repr in1 -> repr in2 -> repr (Ap in1 in2)
x & y = apply_ (apply apply_ x) y

type family Sh (in1 :: *) (in2 :: *) :: *
type instance Sh (result :/ argument) argument = result
type instance Sh argument (argument :\ result) = result
type instance Sh (Tower c d in1) (Tower d e in2) = Tower c e (Sh in1 in2)

class Shift in1 in2 where
  shift :: (Symantics repr) => FMap repr f -> f in1 -> f (Sh in1 in2 :/ in2)

instance (argument ~ argument') => Shift (result :/ argument) argument' where
  shift fmap x = x

instance (d ~ d', Shift in1 in2) => Shift (Tower c d in1) (Tower d' e in2) where
  shift fmap x = fmap scope_ (unT (shift (fmapTower fmap) (T x)))

(#) :: (Symantics repr, Shift in1 in2)
    => repr in1 -> repr in2 -> repr (Sh in1 in2)
f # x = apply_ (shift apply_ f) x

skip :: (Symantics repr) => repr (b :/ a) -> repr (Tower c d b :/ Tower c d a)
skip f = apply_ scope_ (apply_ lift f)

type family Lo (c :: *) (d :: *) (a :: *) :: *
type instance Lo a S S = a
type instance Lo c d (Tower c' d' a) = Tower c d (Lo c' d' a)

class Lower c d a where
  lower_ :: (Symantics repr) => repr (Lo c d a :/ Tower c d a)

instance Lower a S S where
  lower_ = down_

instance (Lower c' d' a) => Lower c d (Tower c' d' a) where
  lower_ = skip lower_

lower :: (Symantics repr, Lower c d a) => repr (Tower c d a) -> repr (Lo c d a)
lower = apply_ lower_

type family Bi (c :: *) (d :: *) (a :: *) :: *
type instance Bi b a DP = Tower b (DP :> a) DP
type instance Bi c d (Tower c' d' a) = Tower c d (Bi c' d' a)

class Bind c d a where
  bind_ :: (Symantics repr) => repr (Bi c d a :/ Tower c d a)

instance Bind b a DP where
  bind_ = bind__

instance (Bind c' d' a) => Bind c d (Tower c' d' a) where
  bind_ = skip bind_

bind :: (Symantics repr, Bind c d a) => repr (Tower c d a) -> repr (Bi c d a)
bind = apply_ bind_

-- Sample derivations

sen1 = lower (everyone & ((lift # like) & someone))
sen2 = lower (lower ((lift # everyone) &
                     ((lift # (lift # like)) & (skip lift # someone))))
sen3 = lower (bind everyone & ((lift # like) & it_))
sen4 = lower (lower ((every & ((lift # farmer) &
                               ((lift # who) & ((lift # own) &
                                                bind (a & donkey))))) &
                     (skip lift # ((lift # like) & it_))))

-- English syntax

data EN a = EN { unEN :: String }

instance Symantics EN where
  john     = EN "John"
  mary     = EN "Mary"
  like     = EN "likes"
  own      = EN "owns"
  farmer   = EN "farmer"
  donkey   = EN "donkey"
  everyone = EN "everyone"
  every    = EN "every"
  someone  = EN "someone"
  a        = EN "a"
  who      = EN "who"
  it_      = EN "it"

  apply_ (EN "") (EN x) = EN x
  apply_ (EN f ) (EN x) = EN (f ++ " " ++ x)

  lift_  = EN ""
  lift   = EN ""
  down_  = EN ""
  scope_ = EN ""
  bind__ = EN ""

instance Show (EN a) where
  show = unEN

-- Lambda semantics

type family Tr (a :: *) :: *
type instance Tr S                     = Bool
type instance Tr DP                    = Entity
type instance Tr N                     = Entity -> Bool
type instance Tr (result :/ argument)  = Tr argument -> Tr result
type instance Tr (argument :\ result)  = Tr argument -> Tr result
type instance Tr (answer :// argument) = Tr argument -> Tr answer
type instance Tr (argument :\\ answer) = Tr argument -> Tr answer
type instance Tr (binder :> answer)    = Tr binder -> Tr answer

data Sem lrepr a = Sem { unSem :: lrepr (Tr a) }

instance (Lambda lrepr) => Symantics (Sem lrepr) where
  john     = Sem john'
  mary     = Sem mary'
  like     = Sem like'
  own      = Sem own'
  farmer   = Sem farmer'
  donkey   = Sem donkey'
  everyone = Sem forall
  every    = Sem (lam (\k -> neg (app exists (lam (\x -> app k
                 (lam (\p -> lam (\k -> conj (app p x) (neg (app k x))))))))))
  someone  = Sem exists
  a        = Sem (lam exists_)
  who      = Sem (lam (\r -> lam (\q -> lam (\x -> conj (app q x) (app r x)))))
  it_      = Sem (lam id)

  apply_   = \(Sem f) (Sem x) -> Sem (app f x)
  lift_    = Sem (lam (\x -> lam (\k -> app k x)))
  lift     = Sem (lam (\x -> lam (\k -> app k x)))
  down_    = Sem (lam (\x -> app x (lam id)))
  scope_   = Sem (lam (\l -> lam (\r -> lam (\k ->
                  app l (lam (\f -> app r (lam (\x -> app k (app f x)))))))))
  bind__   = Sem (lam (\x -> lam (\k -> app x (lam (\x -> app (app k x) x)))))

instance Show (Sem C a) where
  show (Sem x) = show x

instance Show (Sem (P C) a) where
  show (Sem x) = show x
