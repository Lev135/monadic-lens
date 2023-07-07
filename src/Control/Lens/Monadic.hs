module Control.Lens.Monadic (
  -- $about

  -- * #effOptics# What is an effecful optics?
  -- $effOptics

  -- * #order# The order of effects
  -- $order

  -- * #laws# The laws
  -- $laws

  module Control.Lens.Monadic.Iso,
  module Control.Lens.Monadic.Lens,
  module Control.Lens.Monadic.Prism,
  module Control.Lens.Monadic.Traversal,
  module Control.Lens.Monadic.Getter,
  module Control.Lens.Monadic.Setter,
  module Control.Lens.Monadic.Fold
) where

import Control.Lens.Monadic.Fold
import Control.Lens.Monadic.Getter
import Control.Lens.Monadic.Iso
import Control.Lens.Monadic.Lens
import Control.Lens.Monadic.Prism
import Control.Lens.Monadic.Setter
import Control.Lens.Monadic.Traversal

{- $about
  This package provides types and combinators for effectful optics, i.e. optics
  with possible effects. It uses transparent van Laarhoven representation, like
  well known [lens](https://hackage.haskell.org/package/lens) library.
-}

{- $effOptics
  We use the term "effectful" (or sometimes "monadic") optics to describe
  optics-like structures, producing effects while traversing the data.
  Most of the types and functions from this package have analogues from lens,
  for them we use the same name adding the @M@ letter to the end.

  The key properties of effectful optics are:

  - on the value level (if we're ignoring effects) they behave like the pure
    analogues;
  - they can produce effects both while going from a "big" structure to "small"
    (/forward/ effects) and while going back from a "small" structure to "big"
    (/backward/ effects);
  - each effectful optic type is a supertype of the corresponding pure one.
    This means that we can use standard optics as monadic with no need of
    special lifting operation.
-}

{- $order
  Since effectful optics provide side effects the order does matter. To describe
  it we use the notions of /forward/ and /backward/ effects.
  Suppose (possibly) type-changing optics @l :: O m s t a b@ focuses on @a@
  from @s@, changing the focus to @b@ and building a modified structure @t@.
  The effects applying on the way from @s@ to @a@ are called /forward/, those
  on the way from @b@ to @t@ /backward/. For each optic constructor we specify,
  what effects will be treated as /forward/ and /backward/. For optics'
  eliminators --- which kind of effects they apply.

  For example, 'Control.Lens.Monadic.LensM' constructor
  'Control.Lens.Monadic.lensM' has the following description

-}

{- $laws
  The general rule, we try to follow, specifying these laws is that values
  in optics can affect effects, but not vice versa, so at the value level
  pure optics laws should be satisfied. For example, you can write some log
  or report an error, use state for counting some statistics, but the value
  the optic returns (if it does), shouldn't depend on those effects.

  To state laws more precisely we'll use the notion of equality up to (possible)
  effects, i. e. two monadic values of type 'm a' have equivalent inner data
  of type 'a', but can have different effects. In haskell this relation can be
  defined as

  @
    (≈) :: (Eq (m Bool), Applicative m, Eq a) => m a -> m a -> Bool
    ma ≈ mb = (me == (True <$ me))
        where me = (==) \<$> ma \<*> mb
  @

  This notion is abstract and we can rewrite it more clearly for common monads:
  for 'Identity' its equivalent to simple '=='. For others we have @ma ≈ mb@
  iff @ma == mb@ or

    - 'Maybe':  @ma@ or @mb@ is 'Nothing'
    - 'Either': @ma@ or @mb@ is constructed using 'Left'
    - 'List':   all values in @ma@ and @mb@ are equal (though the number
                of elements can differ)
    - 'Control.Monad.Writer.Writer':
        @ma@ and @mb@ have different logs, but the same value
    - 'Control.Monad.Except.Except'
        @ma@ or @mb@ failed

  For the following monads we doesn't actually have @Eq (m Bool)@ instance.
  However, we can think about functional equality like "produces the same output
  for all input". Then we have that @ma ≈ mb@ iff

    - 'Control.Monad.Reader.Reader':
        @ma@ and @mb@ provide equal value for all environments (though
        for different environments these values can differ)
    - 'Control.Monad.State.State':
        @ma@ and @mb@ provide equal value for all initial state with possibly
        different result state
-}
