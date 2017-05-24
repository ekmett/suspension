{-# LANGUAGE GADTs, RankNTypes, DeriveTraversable, ViewPatterns, LambdaCase, KindSignatures, EmptyCase, PolyKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Suspension where

import Bound.Class
import Control.Monad (ap)
import Control.Monad.Trans.Class
import Control.Category
import Prelude hiding (id,(.),gcd)
import Data.TASequence
import Data.TASequence.FastCatQueue

data Var a = B | F a deriving (Functor,Foldable,Traversable)

data Susp :: (* -> *) -> * -> * where
  Susp :: Env f a b -> f a -> Susp f b

data W :: * -> * -> * where
  W :: W a (Var a)

data E :: (* -> *) -> * -> * -> * where
  E :: Susp f a -> E f (Var a) a

type Q = FastTCQueue

data Env f a b where
  Env :: Q (E f) a b -> Q W b c -> Env f a c

instance Category (Env f) where
  id = Env tempty tempty
  Env gcd0 wde0 . Env gab0 wbc0 = go gab0 wbc0 gcd0 wde0 where
    go :: Q (E f) a b -> Q W b c -> Q (E f) c d -> Q W d e -> Env f a e
    go gab wbc gcd wde = case tviewr wbc of 
      TAEmptyR -> Env (gab >< gcd) wde
      wbx :> W -> case tviewl gcd of
        TAEmptyL -> Env gab (wbc >< wde)
        E _ :< gxd -> go gab wbx gxd wde

cat :: (TASequence s, Category d) => (forall a b. c a b -> d a b) -> s c x y -> d x y
cat f xs = case tviewl xs of
  TAEmptyL -> id
  y :< zs -> f y >>> cat f zs

weaken :: Q W a b -> a -> b
weaken = cat (\W -> F)

env :: Monad f => Env f a b -> a -> Susp f b
env (Env gab0 wbc0) a0 = go gab0 wbc0 a0 where
  go :: Monad f => Q (E f) a b -> Q W b c -> a -> Susp f c
  go es ws a = case tviewl es of
    TAEmptyL -> return $ weaken ws a
    E e :< es' -> case a of
      B -> susp (Env es' ws) e
      F a' -> go es' ws a'

env' :: Monad f => Env f a b -> a -> f b
env' (Env gab wbc) a = case tviewl gab of
  TAEmptyL -> return $ weaken wbc a
  E (Susp e m) :< gxb -> case a of
    B -> m >>= env' (Env gxb wbc . e)
    F a' -> env' (Env gxb wbc) a'

susp :: Env f a b -> Susp f a -> Susp f b
susp e (Susp e' m) = Susp (e . e') m

lower :: Monad f => Susp f a -> f a
lower (Susp e m) = m >>= env' e

instance MonadTrans Susp where
  lift = Susp id

instance Monad f => Applicative (Susp f) where
  pure = lift . pure
  (<*>) = ap

instance Monad f => Monad (Susp f) where
 --Susp (Env gab0 wbc0) fa0 >>= cfd0 = go fa0 gab0 wbc0 cfd0 where
 --   go :: Monad f => f a -> Q (E f) a b -> Q W b c -> (c -> Susp f d) -> Susp f d
 --   go fa gab wbc cfd = case tviewl gab of 
      -- E sa :< ga'b -> go _fa' ga'b wbc cfd

instance Bound Susp

instance Functor f => Functor (Susp f) where
  -- fmap cd0 (Susp (Env eab0 wbc0) fa0) = undefined -- go fa0 eab0 wbc0 cd0 where
{-
    go :: Functor f => f a -> Q (E f) a b -> Q W b c -> (c -> d) -> Susp f d
    go fa eab wbc cd = case tviewl eab of
     TAEmptyL -> Susp id $ fmap (cd . weaken wbc) fa
     E (Susp nax ma) :< exb -> case go _fx exb wbc cd of
       Susp nxd fx -> Susp (E <| nxd) let nxc = Env exb wbc in
       E (fmap cd (Susp (nxc.nax) ma)) <| 
  fmap cd (Susp @(Env eab wbc) fa) = case tviewl eab of
    TAEmptyL -> lift $ fmap (cd . weaken wbc) fa -- flushes out weakenings
    E (Susp e m) :< ea'b -> E (fmap cd (Susp (Env ea'b wbc . e) m) <| 
-}

