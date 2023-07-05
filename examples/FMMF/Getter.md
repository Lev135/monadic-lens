Monadic optics and isomorphisms
===
One month ago I wrote a [note][NOTE] about monadic lens, i.e. lens-like
structures providing some effects while viewing/setting. I was asked about the
laws these optics should satisfy. I answered, that we don't need GET-PUT,
PUT-PUT and PUT-GET laws for effectful lenses, since no one would expect them
to satisfy this laws. Thinking a bit, I've realized that these laws should be
applied to the *values* packed in the monad and even discovered how to state
this formally. But then I've realized, that there are some much more important
optics' laws, that should definitely be applied to effectful variants too.
That's why I came back to more simple kind of optics, i.e. getters.

Pure getters
---
You may stop me now: what? getter laws?! In `lens` docs for [Getter][Getter-doc]
we read:

> Since a Getter cannot be used to write back there are no Lens laws that can be
  applied to it.

That's true we can't apply GET-PUT and others to `Getter`s, but this doesn't
mean there're no laws for getters at all. The reason no law is stated for
`Getter` in `lens` library is that all of them doesn't impose any restrictions,
i.e. are free theorems. In fact implicitly they are mentioned in the following
sentence:

> In fact, it is isomorphic to an arbitrary function from (s -> a).

Isomorphism certainly gives us several laws. In `lens` library we have two
main functions for getters (I changed the type of `view` a bit here for
simplicity):

```hs
view :: Getter s a -> s -> a
to   :: (s -> a) -> Getter s a
```

providing an isomorphism between functions of type `s -> a` with composition
and `Getter s a` with (reversed) optics composition:

```hs
to . view ≡ id
view . to ≡ id
to f . to g ≡ to (g . f)
view lg . view lf ≡ view (lf . lg)
```

In fact, the first law above is not quite correct: the reason is that
`to . view` restricts the optics' kind to be a `Getter`. So more accurately we
have

```hs
to . view ≡ asGetter
  where
    asGetter :: Getter s a -- ^ we take arbitrary optics' that can be used
                           -- as getter (e.g. lens)
             -> Getter s a -- ^ but return only a `Getter` anyway
    asGetter l = l
```

Returning to the laws, the second and the third are very easy to check, the last
one follows from others. The first one is much more tricky: we have
```hs
type Getter s a =
  forall f. (Functor f, Contravariant f) =>
    (a -> f a) -> s -> f s

to :: (s -> a) -> Getter s a
to f afa = phantom . afa . f

view :: Getter s a -> s -> a
view l = getConst . l Const
```
and want to prove
```hs
to (view l) ?≡ (l :: Getter s a)
```
Let's substitute the definitions and apply both sides to an arbitrary
`afa :: a -> f a` where `f` is both `Functor` and `Contravariant`:
```hs
to (view l) afa ≡ phantom . afa . getConst . l Const ?≡ l afa
```

Now we should convince ourselves (I don't want to provide a formal
proof here), that `phantom . afa . getConst . l Const` indeed always
behaves like `l afa`. Recall that `l` has a type
`forall f. (Functor f, Contravariant f) => (a -> f a) -> s -> f s`,
i.e. is parametric over the type `f`, so everything it can do is to
apply `a -> f a` to some value depending only on `s` and then
`fmap`/`contramap` it. The latter doesn't change anything, except
the type since `f` is both `Functor` and `Contravariant`. So essentially
we have
```hs
(l :: Getter s a) afa ≡ phantom . afa . get
```
where `get :: s -> a` is actually a `view l` (as one can easily prove).
Substituting this equality into our goal we get
```hs
phantom . afa . getConst . phantom . Const . get ?≡ phantom . afa . get
```
and it's easy to see that `getConst . phantom . Const ≡ id`.

Monadic `Getter`'s. Requirements.
---

We've proved that pure `Getter s a` are isomorphic to functions `s -> a`.
Now we can state the requirements for monadic getter. We need a type
```hs
type GetterM m s a = _
```
with functions
```hs
toM   :: Monad m => (s -> m a) -> GetterM m s a
viewM :: Monad m => Getter m s a -> s -> m a
```
such that the following conditions hold:
```hs
toM . viewM ≡ asGetterM
viewM . toM ≡ id
toM f . toM g ≡ toM (g <=< f)
viewM lg <=< viewM lf ≡ viewM (lf . lg)
```
(of course, the last one will follow from others)

Furthermore, we'd like pure `Getter`s to be the subtype of the monadic ones,
i.e. the following should typecheck:
```hs
liftGetter :: Monad m => Getter s a -> GetterM m s a
liftGetter l = l
```

`GetterFM` and `GetterMF`
---

In the previous [note][NOTE] I've considered two ways of defining `GetterM`
(they are not the only possible of course):
```hs
type GetterFM m s a =
  forall f. (Functor f, Contravariant f) =>
    (a -> f (m a)) -> s -> f (m s)
type GetterMF m s a =
  forall f. (Functor f, Contravariant f) =>
    (a -> m (f a)) -> s -> m (f s)
```
I claimed, that there's no way to make the first one to be a monadic getter.
I was wrong (though we need one more constraint there). And now I can say, that
the second one is unacceptable, since `toM . viewM ≡ asGetterM` law is violated.
The definition of `toMF` and `viewMF` is straight-forward:
```hs
toMF :: Monad m => (s -> m a) -> GetterMF m s a
toMF h amfa = fmap phantom . amfa <=< h
viewMF :: Monad m => GetterMF m s a -> s -> m a
viewMF l = fmap getConst . l (pure . Const)
```
Reasoning like the pure case we get
```hs
toMF (viewMF l) amfa
  ≡ fmap phantom . amfa <=< (fmap getConst . l (pure . Const))
```
but now we are unable to say that
```hs
l amfa ≡ fmap phantom . amfa <=< getM
```
since `l` can apply some effects *after* `amfa` call. Of course, we could just
claim that every lawful `GetterMF` should satisfy this law, but this is very
hard to check in general. Let's look at the following `LensMF`'s constructor,
proposed by one of the readers of my previous note:
```hs
type LensM m s t a b =
  forall f. Functor f => (a -> m (f b)) -> s -> m (f t)

lensM :: (s -> m a) -> (s -> m (b -> t)) -> LensM m s t a b
lensM getter setter amfa s = do
  a <- getter s
  fb <- amfa a
  bt <- setter s
  return $ fmap bt fb
```
`LensM` constructed by this function is not, in general a valid getter, since
effects, provided by `setter` are applied after the recursive call. Therefore
if we compose such lens with some other `lg :: GetterM m a x` we'll get the
following sequence of effects in `viewM (lensM getter setter . lg)`:
```
getter's effects >> lg's effects >> setter's effects
```
while in `viewM (lensM getter setter) >=> viewM lg`:
```
getter's effects >> setter's effects >> lg's effects
```

For my definition of `lensM` (with the type
`(s -> m a) -> (s -> b -> m t) -> LensM m s t a b`) there is no such problem,
since `setter`'s effects are escaped on viewing. However, it would be much
nicer if the type of the `GetterM` ensured getter's laws.

It seems like `GetterFM` should give us what we want. However, to write `toM`
function for it we need extra constraint on `f` allowing us to take `f` outside
of `m`, i.e. `distribute :: m (f a) -> f (m a)`. Of course, `Const x` is not
a distributive functor, but `Const (m x)` can distribute over `m`.
So we get such implementation:
```hs
type GetterFM m s a =
  forall f. (Contravariant f, Functor f, Distributive' m f) =>
    (a -> f (m a)) -> s -> f (m s)

toFM :: Monad m => (s -> m a) -> GetterFM m s a
toFM h f = phantom . distribute' . fmap f . h

viewFM :: Monad m => GetterFM m s a -> s -> m a
viewFM l = getConst . l (Const . pure)

class Distributive' m f where
  distribute' :: m (f a) -> f (m a)
instance Monad m => Distributive' m (Const (m x)) where
  distribute' = Const . join . fmap getConst
```
However, to make this implementation satisfy the subtyping condition "every
`Getter` is a `GetterFM` we can't follow my previous trick and just pack the
composition of `f` and `m` in `Compose`: the reason is that we have
```hs
instance (Functor f, Contravariant g) => Contravariant (Compose f g)
```
but there is no way to provide this instance when `f` is `Contravariant` and
`g` is `Functor`. Of course, in this situation we could provide a new class
```hs
class (Functor f, Contravariant f) => Phantom f
```
and provide `instance Phantom f => Phantom (Compose f g)` since it's definitely
takes place, but there's much cleaner solution, I've seen in Jules Hedges'
[article][JH]: the main property of `Compose f m` is that we have a function
```hs
joinF :: Compose f m (m a) -> Compose f m a
```
So we can provide a class for this method:
```hs
class RightModule m f where
  joinF :: f (m a) -> f a -- it's called `act` in Jules Hedges' article, but
                          -- the name `joinF` seems be more meaningful for me
```
and then define the `GetterM` as
```hs
type GetterM m s a
  = forall f. (Functor f, Contravariant f, Distributive' m f, RightModule m f)
    => (a -> f a) -> s -> f s
```

[NOTE]: https://gist.github.com/Lev135/21a6f1f9f6fe471992603d8895f316e8
[Getter-doc]: https://hackage.haskell.org/package/lens-5.2.2/docs/Control-Lens-Getter.html#t:Getter
[JH]: https://julesh.com/2023/06/28/monadic-lenses-are-the-optic-for-right-monad-modules-iii/
