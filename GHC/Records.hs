{-# LANGUAGE MultiParamTypeClasses, KindSignatures, DataKinds, TypeFamilies #-}

module GHC.Records (Has(..)) where

import GHC.TypeLits (Symbol)

type family GetResult (r :: *) (f :: Symbol) :: *

class t ~ GetResult r f => Has r (f :: Symbol) t where
  getField :: proxy f -> r -> t
