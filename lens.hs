{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Lenses where

import Data.Kind (Type)

-- | The core Lens type, representing a bidirectional accessor
data Lens a b where
  Lens :: { get :: b -> a
          , put :: a -> b -> b
          } -> Lens a b

-- | Type family to represent lens compatibility
type family Compatible (f :: Type) (g :: Type) :: Bool

-- | Type-level proof of lens compatibility
data IsCompatible f g where
  Compatible :: Compatible f g ~ 'True => IsCompatible f g

-- | Paired lens, combining two compatible lenses
data PairedLens a b c where
  PairedLens :: (Compatible f g ~ 'True) =>
                { fst :: Lens a c
                , snd :: Lens b c
                } -> PairedLens a b c

-- | Chained lens, composing two lenses sequentially
data ChainedLens a b c where
  ChainedLens :: { outer :: Lens b c
                 , inner :: Lens a b
                 } -> ChainedLens a b c

-- | Operator for creating a paired lens
(|) :: (Compatible f g ~ 'True) => Lens a c -> Lens b c -> PairedLens a b c
f | g = PairedLens f g

-- | Operator for creating a chained lens
(-->) :: Lens b c -> Lens a b -> ChainedLens a b c
f --> g = ChainedLens f g

-- | Implementation of get for PairedLens
getPaired :: PairedLens a b c -> c -> (a, b)
getPaired (PairedLens f g) c = (get f c, get g c)

-- | Implementation of put for PairedLens
putPaired :: PairedLens a b c -> (a, b) -> c -> c
putPaired (PairedLens f g) (a, b) c = put g b (put f a c)

-- | Implementation of get for ChainedLens
getChained :: ChainedLens a b c -> c -> a
getChained (ChainedLens f g) = get g . get f

-- | Implementation of put for ChainedLens
putChained :: ChainedLens a b c -> a -> c -> c
putChained (ChainedLens f g) a c = put f (put g a (get f c)) c

-- | Lens focusing on the first element of a pair
fstLens :: Lens a (a, b)
fstLens = Lens { get = fst, put = \a (_, b) -> (a, b) }

-- | Lens focusing on the second element of a pair
sndLens :: Lens b (a, b)
sndLens = Lens { get = snd, put = \b (a, _) -> (a, b) }

-- | Type class for claiming compatibility between lenses
class ClaimCompatible f g where
  claimCompatible :: IsCompatible f g

-- | Example of claiming compatibility (this would be done for specific lens types)
instance ClaimCompatible (Lens Int String) (Lens Bool String) where
  claimCompatible = Compatible

-- | Helper function to use a Lens
useLens :: Lens a b -> (b -> a, a -> b -> b)
useLens l = (get l, put l)

-- | Helper function to use a PairedLens
usePairedLens :: PairedLens a b c -> (c -> (a, b), (a, b) -> c -> c)
usePairedLens l = (getPaired l, putPaired l)

-- | Helper function to use a ChainedLens
useChainedLens :: ChainedLens a b c -> (c -> a, a -> c -> c)
useChainedLens l = (getChained l, putChained l)

-- Example usage
type Person = (String, Int)
type Address = (String, String)
type ContactInfo = (Person, Address)

nameLens :: Lens String Person
nameLens = Lens { get = fst, put = \name (_, age) -> (name, age) }

ageLens :: Lens Int Person
ageLens = Lens { get = snd, put = \age (name, _) -> (name, age) }

streetLens :: Lens String Address
streetLens = Lens { get = fst, put = \street (_, city) -> (street, city) }

cityLens :: Lens String Address
cityLens = Lens { get = snd, put = \city (street, _) -> (street, city) }

-- Claiming compatibility (in real use, this would be more specific)
instance ClaimCompatible (Lens String Person) (Lens Int Person) where
  claimCompatible = Compatible

instance ClaimCompatible (Lens String Address) (Lens String Address) where
  claimCompatible = Compatible

-- Creating complex lenses
personLens :: PairedLens String Int Person
personLens = nameLens | ageLens

addressLens :: PairedLens String String Address
addressLens = streetLens | cityLens

-- Chaining lenses
nameInContactInfo :: ChainedLens String Person ContactInfo
nameInContactInfo = fstLens --> nameLens

streetInContactInfo :: ChainedLens String Address ContactInfo
streetInContactInfo = sndLens --> streetLens

-- Example function using lenses
updateName :: String -> ContactInfo -> ContactInfo
updateName newName = snd (useChainedLens nameInContactInfo) newName

updateStreet :: String -> ContactInfo -> ContactInfo
updateStreet newStreet = snd (useChainedLens streetInContactInfo) newStreet
