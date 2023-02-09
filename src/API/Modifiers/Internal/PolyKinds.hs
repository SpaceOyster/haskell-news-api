{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Modifiers.Internal.PolyKinds where

import Data.Type.Bool (If, type (&&))
import Data.Typeable (Proxy (..))
import GHC.Base (Constraint, Symbol)
import GHC.TypeLits.Extended
  ( ErrorMessage (ShowType, Text, (:<>:)),
    TypeError,
  )

type family Elem (a :: k) (l :: [k]) :: Bool where
  Elem a '[] = 'False
  Elem a (a ': as) = 'True
  Elem a (_ ': as) = Elem a as

infix 3 `Elem`

type family HasToBeInList (a :: k) (cols :: [k]) :: Constraint where
  HasToBeInList a cols =
    If
      (Elem a cols)
      (() :: Constraint)
      ( TypeError
          ( 'Text "Type literal '"
              ':<>: 'ShowType a
              ':<>: 'Text "' is not present in '"
              ':<>: 'ShowType cols
              ':<>: 'Text "' type"
          )
      )

type family IsSubset (a :: [k]) (b :: [k]) :: Bool where
  IsSubset '[] _ = 'True
  IsSubset a a = 'True
  IsSubset (a ': as) bs = Elem a bs && IsSubset as bs

type family HasToBeSubset (a :: [k]) (b :: [k]) :: Constraint where
  HasToBeSubset a b =
    If
      (IsSubset a b)
      (() :: Constraint)
      ( TypeError
          ( 'Text "List "
              ':<>: 'ShowType a
              ':<>: 'Text " is not a subset of list "
              ':<>: 'ShowType b
          )
      )

class ReifyBool (b :: Bool) where
  reifyBool :: Proxy b -> Bool

instance ReifyBool 'True where
  reifyBool _ = True

instance ReifyBool 'False where
  reifyBool _ = False
