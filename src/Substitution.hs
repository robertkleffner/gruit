module Substitution where

import Data.Map as Map
import Data.Set as Set



----------------------------------------------------------------------------
-- Substitutions
----------------------------------------------------------------------------

type Subst = Map.Map TypeVar Type

nullSubst = Map.empty

class Types t where
    apply :: Subst -> t -> t
    ftv :: t -> Set.Set TypeVar

instance Types a => Types [a] where
    apply s = map (apply s)
    ftv = Set.unions $ map ftv

instance Types Monotype where
    ftv (TVar t) = Set.singleton t
    ftv (TApp t1 t2) = Set.union (ftv t1) (ftv t2)
    ftv (TSeq ts) = ftv ts
    ftv (TEffectRow es (Just v)) = Set.singleton v
    ftv (TFieldRow fs mv) =
        case mv of
            Just v  -> Set.insert v $ ftv fs
            Nothing -> ftv fs
    ftv (TDotted t) = ftv t
    ftv _ = Set.empty

    apply sub (TVar t) =
        case Map.lookup t sub of
            Nothing -> (TVar t)
            Just t' -> t'
    apply sub (TApp t1 t2) =
        case apply sub t1 of
            TSeq t1s ->
                case apply sub t2 of
                    TSeq t2s ->
                        if length t1s == length t2s
                        then TSeq $ zipWith TApp t1s t2s
                        else error "somehow the variables in a type application were substituted with sequences of different lengths"
                    t2' -> TSeq $ map (\st -> TApp st t2') t1s
            t1' ->
                case apply sub t2 of
                    TSeq t2s -> TSeq $ map (TApp t1') t2s
                    t2' -> TApp t1' t2'
        TApp (apply sub t1) (apply sub t2)
    apply sub (TSeq ts) =
        flatten $ apply sub ts
        where flatten (TSeq ts':ts) = ts' ++ flatten ts
              flatten (t:ts) = t : flatten ts
              flatten [] = []
    apply sub (TEffectRow es (Just v)) =
        case Map.lookup v sub of
            Nothing -> TEffectRow es (Just v)
            Just (TEffectRow es' v') -> TEffectRow (es ++ es') v'
            _ -> error "somehow got non-effect-row substituted for effect row variable"
    apply sub (TFieldRow fs (Just v)) =
        case Map.lookup v sub of
            Nothing -> TFieldRow (apply sub fs) (Just v)
            Just (TFieldRow fs' v') -> TFieldRow (apply sub fs ++ fs') v'
            _ -> error "somehow got non-field-row substituted for field row variable"
    apply sub (TFieldRow fs Nothing) =
        TFieldRow (apply sub fs) Nothing
    apply sub (TDotted t) =
        case apply sub t of
            TSeq ts -> TSeq ts
            TDotted t' -> TDotted t'
            t' -> TDotted t'
    apply sub t = t

instance Types Polytype where
    ftv (Poly ns t) = ftv t `Set.difference` Set.fromList ns
    apply sub (Poly ns t) = Poly ns (apply (foldr Map.delete sub ns) t)

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = Map.map (apply s1) s2 `Map.union` s1