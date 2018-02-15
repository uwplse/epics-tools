{-# LANGUAGE
        MultiParamTypeClasses,
        FlexibleInstances,
        FlexibleContexts,
        DataKinds,
        KindSignatures,
        TypeFamilies,
        TypeOperators,
        UndecidableInstances
    #-}

module Multi
where

import GHC.TypeLits

class ListToMulti a b where
    toMulti' :: [a] -> ([a], b)

    toMulti :: [a] -> b
    toMulti xs = case toMulti' xs of
        ([], result) -> result
        _ -> error $ "toMulti: too many list elements"

instance ListToMulti a () where
    toMulti' xs = (xs, ())

instance ListToMulti a a where
    toMulti' (x0 : xs) = (xs, x0)
    toMulti' _ = error "toMulti' (1): not enough list elements"

instance ListToMulti a (a, a) where
    toMulti' (x0 : x1 : xs) = (xs, (x0, x1))
    toMulti' _ = error "toMulti' (2): not enough list elements"

instance {-# OVERLAPPABLE #-} ListToMulti a b => ListToMulti a (b, a) where
    toMulti' xs =
        let (rest, m) = toMulti' xs
        in case rest of
            x:xs -> (xs, (m, x))
            _ -> error "toMulti' (n+1): not enough list elements"


class MultiToList a b where
    fromMulti' :: b -> [a] -> [a]

    fromMulti :: b -> [a]
    fromMulti m = fromMulti' m []

instance MultiToList a () where
    fromMulti' () acc = acc

instance MultiToList a a where
    fromMulti' x acc = x : acc

instance MultiToList a (a, a) where
    fromMulti' (x0, x1) acc = x0 : x1 : acc

instance {-# OVERLAPPABLE #-} MultiToList a b => MultiToList a (b, a) where
    fromMulti' (m, x) acc = fromMulti' m (x : acc)


data Nat' = Z | S Nat'

type family Multi' (n :: Nat') a where
    Multi' Z a = ()
    Multi' (S Z) a = a
    Multi' (S (S Z)) a = (a, a)
    Multi' (S (S (S n))) a = (Multi' (S (S n)) a, a)

class MultiOps a b where
    multiLen :: b -> a -> Int
    multiRep :: a -> b
    multiGet :: b -> Int -> a
    multiSet :: b -> Int -> a -> b

instance MultiOps a () where
    multiLen _ _ = 0
    multiRep _ = ()
    multiGet _ i = error "multiGet: out of bounds"
    multiSet _ i _ = error "multiSet: out of bounds"

instance MultiOps a a where
    multiLen _ _ = 1

    multiRep x = x

    multiGet x 0 = x
    multiGet _ _ = error "multiGet: out of bounds"

    multiSet _ 0 x = x
    multiSet _ _ _ = error "multiSet: out of bounds"

instance MultiOps a (a, a) where
    multiLen _ _ = 2

    multiRep x = (x, x)

    multiGet (x, _) 0 = x
    multiGet (_, x) 1 = x
    multiGet _ _ = error "multiGet: out of bounds"

    multiSet (x0, x1) 0 x = (x, x1)
    multiSet (x0, x1) 1 x = (x0, x)
    multiSet _ _ _ = error "multiSet: out of bounds"

instance {-# OVERLAPPABLE #-} MultiOps a b => MultiOps a (b, a) where
    multiLen (m, x) _ = 1 + multiLen m x

    multiRep x = (multiRep x, x)

    multiGet (m, x) i
      | i == multiLen m x = x
      | i < multiLen m x = multiGet m i
      | i > multiLen m x = error "multiGet: out of bounds"

    multiSet (m, x) i x'
      | i == multiLen m x = (m, x')
      | i < multiLen m x = (multiSet m i x', x)
      | i > multiLen m x = error "multiSet: out of bounds"

type family NatToNat' (n :: Nat) where
    NatToNat' 0 = Z
    NatToNat' n = S (NatToNat' (n - 1))

type Multi (n :: Nat) a = Multi' (NatToNat' n) a


multiMap :: (ListToMulti b mb, MultiToList a ma) => (a -> b) -> ma -> mb
multiMap f m = toMulti $ map f $ fromMulti m

multiZip :: (ListToMulti c mc, MultiToList a ma, MultiToList b mb) =>
    (a -> b -> c) -> ma -> mb -> mc
multiZip f ma mb = toMulti $ zipWith f (fromMulti ma) (fromMulti mb)

multiMapM :: (Monad m, ListToMulti b mb, MultiToList a ma) => (a -> m b) -> ma -> m mb
multiMapM f m = do
    xs <- mapM f $ fromMulti m
    return $ toMulti xs
