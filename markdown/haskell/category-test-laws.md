# QuickCheck properties from laws of Category Theory (Functor, Monad, etc...)

## The story

There is a strong correspondence between Haskell and Category Theory. Many Haskell
developers tend to learn category theory as theoretical basis of functional programming.
I think category theory can help understand programs and information that
programs process. But I don't think that is the only way. As buddhism says
there are many ways to nirvana.

When I started to learn Haskell programming, I was almost alone at a small
university with some of my friends interested in the topic of Functional
Programming. No special courses were available. Somehow we encountered
the [Real World Haskell](http://book.realworldhaskell.org/read/) and we learnt Haskell
from that book. After a while we started to be more curios, and googled
terms like `Functor`s and `Monad`s, nonetheless to say, we hit
the search result come from `Category Theory`, again no special courses
were provided at the University, so we started to google again. We found
the books and introductory courses, with the same terminology as Haskell
has but totally different kind of description.

Still I miss a good explanation of the correspondence between Haskell and Category Theory.
When a newcomers to Haskell come, and they come. They usually ask me, if they
have to learn CT (abbreviation Category Theory), my answer is no, but sooner
or later the same people ask me the same question: "I don't understand, how definitions
of Monads in category theory correspondens the ones we have in Haskell, could you explain it to me?"

The problem is that these definitions are more or less the same, but you have to
see how you can transform one to the other. I would like to give a short
explanation for that, supposing that the reader knows the definition of a
`Category` and have written some Haskell code, which have included some
`Functor`s and `Monad`s.

To draw the clear picture, I also created a [github repository](https://github.com/category-test-laws)
how the definitions of CT can be written in Haskell, having the goal
in my mind to keep the abstraction level near to the original definitions of Haskell
itself. As a side effect of the project there are `QuickCheck` properties
that the developer can use, to check if his/her `Functor` and/or `Monad` definitions
fulfill the laws. There is no explanation given about Natural Transformation and Adjoint
in this blog post but references can be found in the repository mentioned above.

## Hask Category

 First of all, we have to talk about the Hask category. In Hask objects are types and
the morphism are total functions. This is a simplistic view, but we don't want to complicate
the things. For further reference please check [Edward Kmett on Hask](https://www.youtube.com/watch?v=Klwkt9oJwg0)
As far as I know his implementation is more CT related than I present here.

### Functor

The first confusion with Haskell; in Category Theory functors map one category to the other,
but in Haskell.

    class Functor f where
      fmap :: (a -> b) -> f a -> f b

The `Functor` type class has function with first argument of `a -> b` which is a pure function.
We need to use some parenthesis to have the a step further: `fmap :: (a -> b) -> (f a -> f b)`.
`fmap` is a pure function that transforms a pure function into an other function that works within
the a category that is transformed by the functor.

### newtype Hask a = ...

To be able to talk about the category of Haskell types and functions we need to denote it. The trick here
is to create a type which is isomorphic to the mentioned category above.

    newtype Hask a = Hask { unHask :: a }

    instance Functor Hask where
      fmap f (Hask x) = Hask (f x)

The isomorphism comes from the `Hask` and `unHask` functions. The Hask type is also a functor, doing
no more than wrapping the computed value into the `Hask` constructor. In short, when we see
type `a` it is isomorhic to `Hask a` which is a Functor in the sense of CT and Haskell. This correspondence
will help us to understand and interpret the Monad definition from Category Theory in the
Haskell context.

## Laws and Properties

### Functor laws

Functors in Category Theory are functions from a category to another category, preserving the structures of
the first category in the second category. The preservation is done via, laws that a Functor should
obey. [These laws are the identity and compostion and can be formalized in Haskell.](https://en.wikibooks.org/wiki/Haskell/The_Functor_class#The_functor_laws)

We can go further, formalizing these laws as functions in haskell that returns True if the law is hold
for some given case.

#### Identity

    identityWith :: (Functor f) => (f a -> f a -> Bool) -> f a -> Bool
    identityWith eq fx = id fx `eq` (fmap id fx)

We need to avoid the `Eq` typeclass, to be able to write the first law of a Functor in the most abstract way.
The `eq` parameter provides us a function which can decide if the given two values of a functor are equivalent or not.
The `fx` parameter represents a value with type `f a`. The `id fx` is written in this form to show
the exact eqaulity (or commutative square) of the two sides.

Using the type `Eq` type class we can reuse the `identityWith` function:

    identity :: (Functor f, Eq (f a)) => f a -> Bool
    identity = identityWith (==)

QuickCheck properties can de defined easily from the given function that represents
the identity law, combining with the [`forAll`](https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/src/Test-QuickCheck-Property.html#forAll)
property of QuickCheck.

    identity_with_prop gen_fa eq = forAll gen_fa (identityWith eq)

For more convenience we can reuse the property above, using the `(==)` combinator

    identity_prop gen_fa = identity_with_prop gen_fa (==)

#### Compostion

The second law is the compostion. It requires that the compostion of two functions inside and outside the
functor has to produce the same result. Everything is written out, to draw the exact picture
what components should be equal in the equatation. Same applies for omitting the `Eq` class here,
we would like to be as abstract as possible.

    compositionWith :: (Functor f) => (f c -> f c -> Bool) -> (b -> c) -> (a -> b) -> f a -> Bool
    compositionWith eq f g fx = (fmap (f . g) fx) `eq` ((fmap f) . (fmap g) $ fx)

Reusing the `compostionWith` we can define the `composition` law.

    composition :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
    composition = compositionWith (==)

The corresponding QuickCheck property uses a very good construction, namely the `Gen (Fun a b)` that
represents a function from `a` to `b` and that function is randomly generated.

    composition_with_prop gen_a gen_fbc gen_fab eq
      = forAll gen_a   $ \x ->
        forAll gen_fab $ \(Fun _ fab) ->
        forAll gen_fbc $ \(Fun _ fbc) ->
        compositionWith eq fbc fab x

    composition_prop gen_a gen_fbc gen_fab
      = commposition_with_prop gen_a gen_fbc gen_fab (==)

### Natural transformations

Next step towards the monad definition is the [Natural Transformation](https://wiki.haskell.org/Category_theory/Natural_transformation).

The definition says that, for every type in `Hask`, and for two (endo)functors in `Hask`, we should
select a transformation which converts values from `f a` to `g a`, for every morphism (total function)
`f` in Hask the diagram commutes.

Where does the index `A` of natural transformation go? The definition says that for every `A` there
is a `function`. It leads us where the actual function depends on a type, so natural transformations
are family of functions, but the type system of Haskell does not support this part of the definition
we can just give natural transformations that give the same function for every type `A`:

    -- Rank2Types
    type NatTrans f g = (Functor f, Functor g) => forall a . f a -> g a

In short natural transformations are conversion between functors, having a nice property;
no matter which path we choose:

 * Applying a function `f :: (a -> b)` in the `F` functor which transforms `f a` to `f b` after
   the natural transformation which gives us a value with type `g b`, we get the same as
 * we would transform the value `f a` to `g a` with the natural transformation, and applying the
   given `f :: a -> b` in the `G` functor, we get `g b`

#### In Haskell:

The case for a given value in `NatTrans f g`, `f a` and `a -> b`, we can check if the LHS and RHS of the `eq`
function holds:

    naturalityWith
      :: (Functor f, Functor g)
      => (g b -> g b -> Bool) -> NatTrans f g -> (a -> b) -> f a -> Bool
    naturalityWith eq nt h x = nt (fmap_F h x) `eq` fmap_G h (nt x) where
      fmap_F = fmap
      fmap_G = fmap

The quickcheck property from the definition

    naturalityWith
      :: (Functor f, Functor g, Show a, Show b, Show (f a))
      => Gen (Fun a b) -> Gen (f a) -> (g b -> g b -> Bool) -> NatTrans f g
      -> Property
    naturalityWith gen_fab gen_a eq natTrans =
      forAll gen_fab $ \(Fun _ fab) ->
      forAll gen_a   $ \a ->
      Laws.naturalityWith eq natTrans fab a

Simple examples of natural transformations are `reverse` and `maybeToList`. Which are transforms one
kind of shapes of information into an another one, somehow naturally for the human cognition.

Another example can be found [here](http://blog.sigfpe.com/2008/05/you-could-have-defined-natural.html)

### Monad laws

#### Slogan

There is a slogan:

    A monad is just a monoid in the category of endofunctors, what's the problem?

##### Endofunctor

Let's see, from step to step. The `endofunctor` is a `Functor` that associates a category
`C` with the same `C` category. Basically in Haskell every `Functor` in an `endofunctor`
where the original category is the `Hask` and the result category is the `Hask`, more precisely
a sub-category of `Hask`.

Let's see a concrete example:

    instance Functor Maybe where
      fmap f Nothing  = Nothing
      fmap f (Just x) = Just (f x)

The type of `fmap` is isomorphic to the ones above

    fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap :: (a -> b) -> (Maybe a -> Maybe b)
	fmap :: Hask (a -> b) -> Hask (Maybe a -> Maybe b)
	fmap :: Hask (a -> b) -> Hask (Maybe a) -> Hask (Maybe b)
	fmap :: Hask (a -> b) -> (Maybe a -> Maybe b)

Every `Functor` in Haskell is an `endofunctor` in the category of Haskell types and
(total) functions.

##### Category of endofuctors

In this category the objects are endofunctors and morphism between them are natural
transformations. See [ncatlab](http://ncatlab.org/nlab/show/endofunctor) for more details.

##### A monoid in the category of endofunctors

The endofunctor category has a monoidal structure. One can compose two functors into one,
because they operate on the same objects (which happens to an another category).
This is called called a [Monoidal category](http://ncatlab.org/nlab/show/monoidal+category)

The monoid needs to have an operation that is the composition of two things, the second
one is the identity element. In a monoid of the category of Haskell endofunctors,
these are the natural transformations of `return`, which is the identity element,
and the `join` which is the (tensor) product.

[The full explanation can be found here.](http://stackoverflow.com/questions/3870088/a-monad-is-just-a-monoid-in-the-category-of-endofunctors-whats-the-problem)

#### Laws in Haskell

Two natural transformations `eta` and `mu` are associated with the monoidal structure.

    etaNaturalityWith :: (Functor m, Monad m) => (m b -> m b -> Bool) -> (a -> b) -> a -> Bool
    etaNaturalityWith eq f x = naturalityWith eq (eta . unHask) f (Hask x) where
      eta = return

    muNaturalityWith :: (Functor m, Monad m) => (m b -> m b -> Bool) -> (a -> b) -> m (m a) -> Bool
    muNaturalityWith eq f x = naturalityWith eq (mu . unF2) f (F2 x)  where
      mu = join

The laws for the monad can be found [here](https://en.wikibooks.org/wiki/Haskell/Category_theory#The_monad_laws_and_their_importance) and [here](https://en.wikipedia.org/wiki/Monad_(category_theory)#Formal_definition). These can be formalized as functions. With the same idea used to formalize laws of functors and natural transformations.

    law1With :: (Functor m, Monad m) => (m a -> m a -> Bool) -> m (m (m a)) -> Bool
    law1With eq mmmx = mu (mu mmmx) `eq` mu (fmap mu mmmx) where
      mu = join

    law2With :: (Functor m, Monad m) => (m a -> m a -> Bool) -> m a -> Bool
    law2With eq mx = mu (eta mx) `eq` mx where
      mu  = join
      eta = return

    law3With :: (Functor m, Monad m) => (m a -> m a -> Bool) -> m a -> Bool
    law3With eq mx = mu (fmap eta mx) `eq` mx where
      mu  = join
      eta = return

Properties from functions are:

    etaNaturalityWith gen_fab gen_a eq =
      forAll gen_fab $ \(Fun _ fab) ->
      forAll gen_a   $ \a ->
      Laws.etaNaturalityWith eq fab a

    muNaturalityWith gen_fab gen_mma eq =
      forAll gen_fab $ \(Fun _ fab) ->
      forAll gen_mma $ \mma ->
      Laws.muNaturalityWith eq fab mma

    law1With gen_mmma eq =
      forAll gen_mmma $ Laws.law1With eq

    law2With gen_ma eq =
      forAll gen_ma $ Laws.law2With eq

    law3With gen_ma eq =
      forAll gen_ma $ Laws.law3With eq

The representation above uses the Haskell representation of monads. It is beneficial, as
no extra work is needed to include properties to test your monad implementation.

## Github repository

The github [category-test-laws](https://github.com/andorp/category-test-laws) repository
has similar codes, but it evolves as I have free time to add more and more
constructions from the Category Theory.
