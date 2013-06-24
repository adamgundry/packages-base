{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

module GHC.Records (Has(..)) where

import GHC.TypeLits

class Has r (x :: Symbol) t where
  getField :: r -> t
