{-# LANGUAGE MultiParamTypeClasses, KindSignatures, DataKinds,
             TypeFamilies, ExplicitForAll #-}

module GHC.Records where

import GHC.TypeLits (Symbol)

type family GetResult (r :: *) (f :: Symbol) :: *

class t ~ GetResult r f => Has r (f :: Symbol) t where
  getField :: proxy f -> r -> t
  setField :: proxy f -> r -> t -> r

class Accessor (p :: * -> * -> *) where
  accessor :: (r -> t) -> (r -> t -> r) -> p r t

instance Accessor (->) where
  accessor = const

field :: forall proxy f r t p . (Has r f t, Accessor p) => proxy f -> p r t
field z = accessor (getField z) (setField z)
