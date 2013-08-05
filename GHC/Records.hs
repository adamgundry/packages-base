{-# LANGUAGE MultiParamTypeClasses, KindSignatures, DataKinds,
             TypeFamilies, RankNTypes, FlexibleInstances, FlexibleContexts #-}

module GHC.Records where

import GHC.TypeLits (Symbol)

type family GetResult (r :: *) (f :: Symbol) :: *

class (t ~ GetResult r f, r ~ SetResult r f t) =>
          Has r (f :: Symbol) t where
  getField :: proxy f -> r -> t

type family SetResult (r :: *) (f :: Symbol) (t :: *) :: *

class Has r f (GetResult r f) => Upd (r :: *) (f :: Symbol) (t :: *) where
  setField :: proxy f -> r -> t -> SetResult r f t


class Accessor (p :: * -> * -> *) (f :: Symbol) where
  accessor :: proxy f -> (r -> GetResult r f) ->
              (forall t . Upd r f t => r -> t -> SetResult r f t) ->
              p r (GetResult r f)

instance Accessor (->) f where
  accessor _ getter _ = getter


field :: forall proxy f r t p . (Has r f t, Accessor p f) => proxy f -> p r t
field z = accessor z (getField z) (setField z)
