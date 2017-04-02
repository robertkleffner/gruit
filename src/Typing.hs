module Typing where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Core



----------------------------------------------------------------------------
-- Substitutions
----------------------------------------------------------------------------

type Subst = Map.Map TypeVar Type

nullSubst = Map.empty

(|->) :: TypeVar -> Type -> Subst
v |-> t = Map.singleton v t

class Types t where
    apply :: Subst -> t -> t
    ftv :: t -> Set.Set TypeVar

instance Types TypeExpr where
    ftv (TVar t) = Set.singleton t
    ftv (TApp t1 t2) = Set.union (ftv t1) (ftv t2)
    ftv _ = Set.empty

    apply sub (TVar t) =
        case Map.lookup t sub of
            Nothing -> (TVar t)
            Just t' -> t'
    apply sub (TApp t1 t2) = TApp (apply sub t1) (apply sub t2)

instance Types Type where
    ftv (Type t) = ftv t
    apply sub (Type t) = Type $ apply sub t

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv = Set.unions $ map ftv

instance Types Constraint where
    ftv (Constraint n ts) = ftv ts
    apply sub (Constraint n ts) = Constraint n (apply sub ts)

instance Types Constrained where
    ftv (Constrained cs t) = ftv cs `Set.union` ftv t
    apply sub (Constrained cs t) = Constrained (apply sub cs) (apply sub t)

instance Types Scheme where
    ftv (Scheme ns t) = ftv t `Set.difference` Set.fromList ns
    apply sub (Scheme ns t) = Scheme ns (apply (foldr Map.delete sub ns) t)