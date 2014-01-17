{-# LANGUAGE MultiParamTypeClasses, KindSignatures, DataKinds,
             TypeFamilies, RankNTypes, FlexibleInstances, FlexibleContexts,
             NoImplicitPrelude, EmptyDataDecls, MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Records
-- Copyright   :  (c) Adam Gundry, 2013
-- License     :  BSD-style (see libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- This is an internal GHC module that defines classes relating to
-- the OverloadedRecordFields extension.
--
-----------------------------------------------------------------------------

{-
Note [Dependency on GHC.Records]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module must be compiled before any module that declares a record
field, because the class declarations below are loaded in order to
generate the supporting definitions for overloaded record fields. To
achieve this, this module is imported by GHC.Base. If you receive the
error "Failed to load interface for ‛GHC.Records’" while compiling
base, this module has not been compiled early enough.
-}

module GHC.Records where

import GHC.Integer ()
import GHC.Prim (Proxy#)

-- | (Kind) This is the kind of type-level symbols.
data Symbol


{-
The OverloadedRecordFields extension generates instances for the
following type classes ('Has' and 'Upd') and type families
('FldTy' and 'UpdTy'). For example, the datatype

    data T a = MkT { foo :: [a] }

gives rise to the instances

    type instance FldTy (T a) "foo"     = [a]
    type instance UpdTy (T a) "foo" [c] = T c
    instance Has (T a) "foo"
    instance b ~ [c] => Upd (T a) "foo" b
-}


-- | @FldTy r n@ is the type of the field @n@ in record type @r@.
type family FldTy (r :: *) (n :: Symbol) :: *
-- See Note [Why not associated types]

-- | @UpdTy r n t@ is the record type that results from setting
-- the field @n@ of record type @r@ to @t@.
type family UpdTy (r :: *) (n :: Symbol) (t :: *) :: *

-- | @Has r n@ means that @r@ is a record type with a field @n@.
class Has r (n :: Symbol) where
  -- | Polymorphic field selector
  getField :: Proxy# n -> r -> FldTy r n

-- | @Upd n r t@ means that @r@ is a record type with field @n@ which
-- can be assigned type @t@.
class ( Has r n
      , r ~ UpdTy r n (FldTy r n)  -- See Note [Superclasses of Upd]
      ) => Upd (r :: *) (n :: Symbol) (t :: *) where
  -- | Polymorphic field update
  setField :: Proxy# n -> r -> t -> UpdTy r n t


{-
Note [Syntactic sugar for Has constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The syntactic sugar

    r { f :: t }

translates to the pair of constraints

    (Has r "f", t ~ FldTy r "f")


Note [Why not associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'FldTy' could be an associated type, but 'UpdTy' cannot, so for
consistency both are separate top-level type families.  The parameters
of associated types must be exactly the same as the class header (they
cannot be more specific instances), so this is currently illegal:

    instance t ~ [b] => Upd (T a) "foo" t where
        type UpdTy (T a) "foo" [b] = T b

If this were allowed, both type families could become associated
types. See Trac #8161. The difference is minimal, however.


Note [Superclasses of Upd]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclasses of 'Upd' ensure that there is always a corresponding
'Has' instance, and that the invariant

    r ~ UpdTy r n (FldTy r n)

always holds. This says that setting a field without changing its type
does not change the type of the record. It is included so that

    [Given] Upd r n (FldTy r n)

implies

    setField :: Proxy# n -> r -> GetField r n -> r

which may make it easier to write some very polymorphic code to update
fields. If you can think of a concrete example of why this is useful,
please add it here!
-}


-- | @Accessor p r n@ means that @p@ is a type into which a field with
-- name @n@ in record @r@ can be translated.  The canonical instance
-- is for the function space (->), which just returns the getter
-- (completely ignoring the setter).  Lens libraries may give instances
-- of 'Accessor' so that overloaded fields can be used as lenses.
class Accessor (p :: * -> * -> *) (r :: *) (n :: Symbol) where
  -- | @accessField z getter setter@ injects a getter and setter pair into @p@
  accessField :: Proxy# n ->
                 (Has r n => r -> FldTy r n) ->
                 (forall t . Upd r n t => r -> t -> UpdTy r n t) ->
                 p r (FldTy r n)

instance Has r n => Accessor (->) r n where
  accessField _ getter _ = getter


{-
When the OverloadedRecordFields extension is enabled, a field @foo@ in
an expression is translated into

    field (proxy# :: Proxy# "foo") :: Accessor p r "foo" => p r (FldTy r n)
-}

-- | Target for translation of overloaded record field occurrences
field :: forall p r n . Accessor p r n => Proxy# n -> p r (FldTy r n)
field z = accessField z (getField z) (setField z)
