{-# LANGUAGE MultiParamTypeClasses, KindSignatures, DataKinds,
             TypeFamilies, RankNTypes, FlexibleInstances, FlexibleContexts,
             NoImplicitPrelude, EmptyDataDecls #-}

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
generate the supporting definitions for overloaded record fields.  If
you receive the error "Failed to load interface for ‛GHC.Records’"
while compiling base, this module has not been compiled early enough.
-}

module GHC.Records where

import GHC.Integer ()

-- | (Kind) This is the kind of type-level symbols.
data Symbol


{-
The OverloadedRecordFields extension generates instances for the
following type classes ('Has' and 'Upd') and type families
('GetResult' and 'SetResult'). For example, the datatype

    data T a = MkT { foo :: [a] }

gives rise to the instances

    type instance GetResult (T a) "foo"     = [a]
    type instance SetResult (T a) "foo" [b] = T b
    instance Has (T a) "foo" [a]
    instance t ~ [b] => Upd (T a) "foo" t
-}


-- | @GetResult r f@ is the type of the field @f@ in record type @r@.
type family GetResult (r :: *) (f :: Symbol) :: *

-- | @SetResult r f t@ is the record type that results from setting
-- the field @f@ of record type @r@ to @t@.
type family SetResult (r :: *) (f :: Symbol) (t :: *) :: *

-- | @Has r f t@ means that @r@ is a record type with field @f@ of type @t@.
class t ~ GetResult r f  -- See Note [Functional dependency via equality superclass]
          => Has r (f :: Symbol) t where
  -- | Polymorphic field selector
  getField :: proxy f -> r -> t

-- | @Upd r f t@ means that @r@ is a record type with field @f@ which
-- can be assigned type @t@.
class (Has r f (GetResult r f), r ~ SetResult r f (GetResult r f))
              -- See Note [Superclasses of Upd]
          => Upd (r :: *) (f :: Symbol) (t :: *) where
  -- | Polymorphic field update
  setField :: proxy f -> r -> t -> SetResult r f t


{-
Note [Functional dependency via equality superclass]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The third parameter of the 'Has' class (the field type) is
functionally dependent on the first two (the record type and field
name), but is present to allow for syntactic sugar:

    r { f :: t }    translates to    Has r "f" t

The functional dependency is encoded using the 'GetResult' type
family, via the equality superclass 't ~ GetResult r f' in the
declaration of 'Has'. Thanks to this superclass, if we have a
constraint

    [Wanted] Has (T alpha) "foo" beta

then we get

    [Derived] beta ~ GetResult (T alpha) "foo".

Now substituting for 'beta' in the wanted constraint and reducing
'GetResult' gives

    [Wanted] Has (T alpha) "foo" [alpha]

so the constraint is solved via

    instance Has (T a) "foo" [a].

On the other hand, the third parameter of 'Upd' is not functionally
dependent on the first two, because it represents the new type being
assigned to the field, not its current type. Thus we must generate

    instance t ~ [b] => Upd (T a) "foo" t

to ensure that a constraint like

    [Wanted] Upd (T alpha) "foo" beta

will be solved.

'GetResult' could be an associated type, but 'SetResult' cannot, so
for consistency both are separate top-level type families.  The
parameters of associated types must be exactly the same as the class
header (they cannot be more specific instances), so this is currently
illegal:

    instance t ~ [b] => Upd (T a) "foo" t where
        type SetResult (T a) "foo" [b] = T b

If this were allowed, both type families could become associated
types. See Trac #8161. The difference is minimal, however.


Note [Superclasses of Upd]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The superclasses of 'Upd' ensure that there is always a corresponding
'Has' instance, and that the invariant

    r ~ SetResult r f (GetResult r f)

always holds. This says that setting a field without changing its type
does not change the type of the record. It is included so that

    [Given] Upd r f (GetResult r f)

implies

    setField :: proxy f -> r -> t -> r

which may make it easier to write some very polymorphic code to update
fields. If you can think of a concrete example of why this is useful,
please add it here!
-}


-- | @Accessor p f@ means that @p@ is a type into which a field with
-- name @f@ can be translated.  The canonical instance is for the
-- function space (->), which just returns the getter (completely
-- ignoring the setter). Lens libraries may give instances of
-- 'Accessor' so that overloaded fields can be used as lenses.
class Accessor (p :: * -> * -> *) (f :: Symbol) where
  -- | @accessor z getter setter@ injects a getter and setter pair into @p@
  accessor :: proxy f -> (r -> GetResult r f) ->
              (forall t . Upd r f t => r -> t -> SetResult r f t) ->
              p r (GetResult r f)

instance Accessor (->) f where
  accessor _ getter _ = getter


{-
When the OverloadedRecordFields extension is enabled, a field @foo@ in
an expression is translated into

    field (Proxy :: Proxy "foo") :: (Has r "foo" t, Accessor p "foo") => p r t
-}

-- | Target for translation of overloaded record field occurrences
field :: forall proxy f r t p . (Has r f t, Accessor p f) => proxy f -> p r t
field z = accessor z (getField z) (setField z)
