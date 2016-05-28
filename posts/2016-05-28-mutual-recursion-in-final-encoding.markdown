---
title: Mutual Recursion in Final Encoding
category: programming
tags: [haskell]
---

Not long ago I was introduced to *final encoding* of [embedded domain specific
languages (EDSLs)](https://wiki.haskell.org/Embedded_domain_specific_language)
in Haskell thanks to a [coding
puzzle](http://www.codewars.com/kata/5424e3bc430ca2e577000048) over at
[CodeWars](www.codewars.com). While solving that puzzle I started wondering
whether it was possible to define mutually recursive functions in an embedded
language in final encoding. In this article we will see that it is indeed
possible and develop a generic and type-safe implementation.

<!--more-->

A brief disclaimer: Many of the concepts that are presented in this article I
only learned about very recently myself. If you find any errors or inaccuracies
then please let me know.

You can find a source-code repository with example codes
[here](https://github.com/aherrmann/final-encoding-examples).

## Initial Encoding

An area in which Haskell really shines is the implementation of embedded domain
specific languages. The most straight-forward way of implementing such a
language is to define a data-type that can represent the [abstract syntax
tree (AST)](https://en.wikipedia.org/wiki/Abstract_syntax_tree) of expressions
in the embedded language.

Consider a very simple example language that allows to express addition and
negation of integers. We could represent the AST as follows.

``` haskell
data Expr = Lit Int        -- literal value
          | Neg Expr       -- negation
          | Add Expr Expr  -- addition
```

A simple expression such as `1 + (2 - 3)` would then be represented by the
following value.

``` haskell
exprI = Add (Lit 1) (Add (Lit 2) (Neg (Lit 3)))
```

We can then write an interpreter which evaluates the expression and turns it
into a Haskell integer.

``` haskell
eval :: Expr -> Int
eval (Lit n) = n
eval (Neg x) = negate (eval x)
eval (Add a b) = (eval a) + (eval b)
```

Lo and behold, if we apply it to our simple example expression then we get the
expected answer.

``` haskell
>>> eval exprI
0
```

Having a value level representation of an expression allows us to define
different interpreters for it. E.g. a pretty-printer that generates a textual
representation, or we could compile the expression and run it on a GPU (of
course only sensible for much larger computations).

This kind of encoding of an embedded language is called *initial encoding*.
Unfortunately, there are a few downsides to this approach. In short, extending
the language with new capabilities (e.g. multiplication) will require a change
of the data type `Expr` and will thereby invalidate all code that operates on
that type. At the very least, every module will have to be recompiled.
Furthermore, large expressions will be represented by large nested data
structures. That might cause performance problems.

## Final Encoding

A different way of encoding embedded languages is the so called *final
encoding*. If you're not familiar with that approach then I strongly recommend
reading Oleg Kiselyov's [lecture notes
(PDF)](http://okmij.org/ftp/tagless-final/course/lecture.pdf) on the topic.
Additionally, it's well worth it to go through the accompanying [example
codes](http://okmij.org/ftp/tagless-final/course/index.html#lecture). The
embedded language that we define below is based on some of these example codes.

Here, I will only give a brief summary of the concept. In contrast to initial
encoding, final encoding does not represent expressions in the embedded
language as values of a dedicated data-type. Instead it makes rather clever use
of Haskell's type-classes. The syntax of the embedded language is defined
through functions in type-classes. Expressions in the embedded language are
then formed by application of these functions. Interpreters are written as
instances of the type-classes

To give an example: The simple arithmetic language from above could be defined
as follows.

``` haskell
class AddLang repr where
    int :: Int -> repr Int
    neg :: repr Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
```

This means that the language supports three primitives `int`, `neg`, `add`. The
example expression from above would then be written as follows.

``` haskell
exprF = add (int 1) (add (int 2) (neg (int 3)))
```

To evaluate that expression we will provide a corresponding interpreter. An
interpreter is implemented as an instance of the class that defines the
language.

``` haskell
newtype Eval a = Eval { eval :: a }

instance AddLang Eval where
    int = Eval
    neg x = Eval $ negate (eval x)
    add a b = Eval $ (eval a) + (eval b)
```

Here we define a representation `Eval` which is just a `newtype`-wrapper around
any Haskell value. To construct an `int` expression from a Haskell integer we
just apply the data-constructor `Eval`. To negate an expression we first
evaluate it, apply Haskell's `negate` function, and then pack it up again.
Similarly, `add` applies Haskell's `(+)` on `a`'s and `b`'s evaluations and
packs the result. Since `newtype`-wrappers don't incur any runtime overhead,
evaluating an expression of the embedded language is just as efficient as
evaluation of an equivalent Haskell expression.

The expression still yields the same result.

``` haskell
>>> eval exprF
0
```

The above implementation can be made more concise if we notice that `Eval` is
an `Applicative`.

``` haskell
-- We could use the DeriveFunctor extension instead
instance Functor Eval where
    fmap f x = Eval $ f (eval x)

instance Applicative Eval where
    pure = Eval
    f <*> x = Eval $ (eval f) (eval x)

instance AddLang repr where
    int = pure
    neg = liftA negate
    add = liftA2 (+)
```

Extensions of the embedded language can be provided simply by adding new
type-classes. E.g. for multiplication.

``` haskell
class MulLang repr where
    mul :: repr Int -> repr Int -> repr Int
```

In order to evaluate expressions that mention `mul` we will also have to extend
the interpreter.

``` haskell
instance MulLang Eval where
    mul = liftA2 (*)
```

But, that this doesn't invalidate any of the previously existing code. In final
encoding the language is easily extensible.

## Defining the Language

For the remainder of this article we will assume the following language
definition and then expand upon that. It is a form of [simply typed lambda
calculus](https://en.wikipedia.org/wiki/Simply_typed_lambda_calculus) with some
simple arithmetic, Boolean logic, and branching on top of it. For convenience
we will also add subtraction as a primitive to the language.

``` haskell
class AddLang repr where
    int :: Int -> repr Int
    neg :: repr Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    sub :: repr Int -> repr Int -> repr Int

class MulLang repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolLang repr where
    bool :: Bool -> repr Bool
    not :: repr Bool -> repr Bool
    and :: repr Bool -> repr Bool -> repr Bool
    or :: repr Bool -> repr Bool -> repr Bool

class EqLang repr where
    eq :: Eq a => repr a -> repr a -> repr Bool
    neq :: Eq a => repr a -> repr a -> repr Bool

class IfLang repr where
    if_ :: repr Bool -> repr a -> repr a -> repr a

class LambdaLang repr where
    lam :: (repr a -> repr b) -> repr (a -> b)
    ap :: repr (a -> b) -> repr a -> repr b
```

The full implementation of the evaluation interpreter is available with the
[code
examples](https://github.com/aherrmann/final-encoding-examples/blob/85d0b8f0dff6d8cb817743be3a05606b0a452712/src/Ex03_Language.hs)
to this article. Here I will only show the implementation of `IfLang` and
`LambdaLang` as those are the only ones that differ significantly from the
previous `AddLang` implementation.

``` haskell
instance IfLang Eval where
    if_ cond then_ else_ = Eval $ if (eval cond)
                                    then (eval then_)
                                    else (eval else_)

instance LambdaLang Eval where
    lam f = Eval $ eval . f . Eval
    ap = (<*>)
```

Note, that we are placing the burden of type-checking and name-binding on the
host language (Haskell). Haskell's type-checker prohibits us from accidentally
adding a Boolean value to an integer. It is also impossible to write a function
that refers to a non-existent argument.

As a simple example we will consider a function that takes a number $n$ and
checks whether that number fulfills the equation $n^2 = 5^2$.

``` haskell
-- type can be inferred (constraints omitted)
fun :: repr (Int -> Bool)
fun = lam (\n -> (n `mul` n) `eq` (int 5 `mul` int 5))
```

We can then apply that function to a value and see the result.

``` haskell
>>> eval $ ap fun (int 4)
False

>>> eval $ ap fun (int 5)
True
```

In itself that might not seem so exiting, as we could just as well have
implemented that function in Haskell right away. However, we are again free to
provide other interpreters for the language. E.g. a pretty-printer, or a GPGPU
compiler. In the [example
codes](https://github.com/aherrmann/final-encoding-examples/blob/85d0b8f0dff6d8cb817743be3a05606b0a452712/src/Ex03_Language.hs#L81)
you can find a simple pretty-printer. It turns the function `fun` from above
into the following textual representation.

``` haskell
>>> putStrLn $ pretty fun
(\x0 -> ((x0 * x0) == (5 * 5)))
```

## Recursion and the Fixed-Point Combinator

So far, the embedded language allows to define functions and do a bit of
arithmetic, and logic. Next, we want to be able to express recursive functions.
We will consider the factorial function. In Haskell we could define that
function as follows.

``` haskell
facH n = if n == 0 then 1 else n * facH (n-1)
```

In this definition we use Haskell's recursive let-bindings. The embedded
language does not allow any let-bindings so far. Therefore, we have to take a
different approach. First, we will rewrite the factorial function in open
recursive form.

``` haskell
open_fac self n = if n == 0 then 1 else n * self (n-1)
```

Then, we can apply the [fixed-point
combinator](https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion) to close
it and obtain the factorial function again.

``` haskell
fixH :: (a -> a) -> a
fixH f = f (fixH f)

facH2 = fixH open_fac
```

## The Fixed-Point Combinator in the Embedded Language

First, we provide the fixed-point combinator as a primitive in the embedded
language.

``` haskell
class FixLang repr where
    fix :: (repr a -> repr a) -> repr a

instance FixLang Eval where
    fix f = f (fix f)
```

With it we can now implement the factorial function within the embedded
language.

``` haskell
fac = fix ( \self -> lam ( \n ->
              if_ (n `eq` (int 0))
                  (int 1)
                  (n `mul` (ap self (n `sub` (int 1))))
            )
          ) 
```

``` haskell
>>> eval $ ap fac (int 0)
1
>>> eval $ ap fac (int 3)
6
>>> eval $ ap fac (int 5)
120
>>> putStrLn $ pretty fac
fix (\self0 -> (\x1 -> (if (x1 == 0) then 1 else (x1 * (self0 (x1 - 1))))))
```

Great! We have a working factorial function.

## Mutual Recursion

Now, let us consider mutually recursive functions. A simple example are the
functions `even` and `odd` which determine whether a given positive number is
even or odd. A mutually recursive implementation in Haskell could look like
this.

``` haskell
even n = n == 0 || odd (n-1)
odd  n = n /= 0 && even (n-1)
```

We use that zero is an even number and therefore, if the number is zero then it
cannot be odd. Furthermore, if `n` is even, then its predecessor `n-1` is odd
and vice-versa. These functions are only indirectly recursive: The function
`odd` only calls itself indirectly through `even`. However, it is not possible
to define either of those functions without the other (in that way). Haskell
allows mutually recursive let-bindings. The embedded language does not, yet.

As before we will first consider an open recursion form of the above functions.
A first, naïve approach might look as follows.

``` haskell
-- type can be inferred
evodH1 :: (Int -> Bool, Int -> Bool) -> (Int -> Bool, Int -> Bool)
evodH1 (ev, od) = ( \n -> n == 0 || od (n-1)
                  , \n -> n /= 0 && ev (n-1) )
```

Here, `evodH1` is a function that takes a pair of functions `ev` and `od` as
the first argument. It then returns a pair of the functions `even` and `odd`.
Looking at its type-signature we see that we can apply the fixed-point
combinator to this function.

``` haskell
(ev1,od1) = fixH evodH1
```

If we actually try to use any of those two functions then we will need to bring
a lot of patience along. They will both loop forever.

If we insert the definition for `fixH` this quickly becomes obvious.

``` haskell
fixH f = f (fixH f)

(ev,od) = fixH evodH1
        = evodH1 (fixH evodH1)
        = evodH1 (evodH1 (fixH evod1))
        = ...
```

The issue is that we never actually reach a point where we can split up the
pair. For that we would need to fully evaluate the infinite tower of `evodH1`'s
first. To be more precise it is neither `ev`, nor `od` itself that is looping
forever. Instead, it is the lazy pattern binding `(ev,od)`. We need to find a
better implementation of `evodH1`, one that allows to split the pair
immediately.

``` haskell
evodH2 :: ( (Int -> Bool, Int -> Bool) -> (Int -> Bool)
          , (Int -> Bool, Int -> Bool) -> (Int -> Bool) )
evodH2 = ( \(ev,od) -> \n -> n == 0 || od (n-1)
         , \(ev,od) -> \n -> n /= 0 && ev (n-1) )
```

Much better. Unfortunately, we can no longer apply the previously defined
fixed-point combinator. We need to define a new combinator for a pair of
mutually recursive functions.

``` haskell
fixH2 :: ( (a, b) -> a
         , (a, b) -> b )
      -> (a, b)
fixH2 fs@(a, b) = (a (fixH2 fs), b (fixH2 fs))
```

If we apply this combinator to `evodH2` we will get a pair of working
functions.

``` haskell
>>> let (ev,od) = fixH2 evodH2
>>> ev 4
True
>>> ev 3
False
>>> od 3
True
>>> od 2
False
```

Very nice, we were able to express `even` and `odd` in open recursive form and
close it through a mutual recursion fixed-point combinator.  But, how would we
generalize this solution to an arbitrary number of mutually recursive
functions? One solution that [one can find
online](http://stackoverflow.com/a/18413448/841562) relies on lists instead of
tuples.

``` haskell
fixL :: [[a] -> a] -> [a]
fixL fs = map (\f -> f (fixL fs)) fs

evodL = [ \[ev,od] n -> n == 0 || od (n-1)
        , \[ev,od] n -> n /= 0 && ev (n-1) ]
```

That approach would work for `even` and `odd`. However, there are a number of
downsides. First, it's unsafe. There is nothing preventing us from accessing a
(non-existent) third element of the list which is taken as the first argument.
Second, lists are homogeneous, i.e. all elements must have the same type. There
is no (generic) way to find the fixed-point of mutually recursive functions
that have different types.

Consider the following contrived example.

``` haskell
anInt = if aBool then 1 else 0
aBool = anInt == 0
```

This is a pair of mutually recursive expressions that both have different
types. The first one is an integer, the second a Boolean value. Of course both
of them don't terminate. But, we could not express them in open recursion form
and close them with `fixL` because we cannot construct a homogeneous list of an
integer and a Boolean value.

## Heterogeneous Lists

Instead of having a fixed-point combinator that takes a homogeneous list, or a
pair of values, we need one that takes a heterogeneous collection. In this
article we will use a heterogeneous list for that purpose.

The [`HList` package](http://hackage.haskell.org/package/HList) by Oleg
Kiselyov et. al. is a good resource on how to implement heterogeneous lists in
Haskell. They describe two different ways that are relevant for our use case.

### Generalized Algebraic Data Types

One uses [generalized algebraic data types
(GADTs)](https://en.wikibooks.org/wiki/Haskell/GADT) and looks as follows.

``` haskell
data HListG xs where
    HNilG :: HListG '[]
    HConsG :: x -> HListG xs -> HListG (x ': xs)
```

This uses the `DataKinds`, and `TypeOperators` extensions to express lists on
the type level. The apostrophe in `'[]` and `x ': xs` lifts those symbols from
the value-level to the type-level such that e.g. `Char ': '[Bool]` would be the
type-level list `'[Char, Bool]`. Of course it also requires the `GADTs`
extension. With this implementation we could form a heterogeneous list like so.

``` haskell
-- type can be inferred
listG :: HListG '[Char, Bool]
listG = HConsG 'a' (HConsG True HNilG)
```

We can define recursive functions on such lists as follows. E.g. to calculate
the length of a given list.

``` haskell
-- type cannot be inferred
hLengthG :: HListG xs -> Int
hLengthG HNilG = 0
hLengthG (HConsG _ xs) = 1 + hLengthG xs
```

The downside of using GADTs to represent heterogeneous lists is that they
[don't allow lazy pattern
bindings](http://stackoverflow.com/questions/14538255/why-gadt-existential-data-constructors-cannot-be-used-in-lazy-patterns)
on the one hand, and on the other hand strongly limit the extent of type
inference wherever they appear. This is discussed in some more detail in [this
StackOverflow
thread](http://stackoverflow.com/questions/28582210/type-inference-with-gadts-a0-is-untouchable).
As an example, the type of the following expression cannot be inferred.

``` haskell
-- type cannot be inferred
mysteriousG :: HListG '[Char, Bool] -> (String, Bool)
mysteriousG = \(HConsG c (HConsG b HNilG)) -> (c:"", not b)
```

This is particularly inconvenient for our use case. In the end we will need to
apply our mutual recursion fixed-point combinator to a list of functions that
each of them take a heterogeneous list as an argument. Having to provide type
signatures for every set of mutually recursive functions that we want to define
would not make for a good user experience.

### Data Families

Another option is to use [data
families](https://wiki.haskell.org/GHC/Type_families#Detailed_definition_of_data_families).
They require the `TypeFamilies` extension and offer a more functional approach
to define data types. Below we define `HListF` as a type-level function that
expects a list of types and returns a type.

``` haskell
data family HListF :: [*] -> *
data instance HListF '[] = HNilF
data instance HListF (x ': xs) = HConsF x (HListF xs)
```

We pattern match on the empty list, in which case the corresponding
data-constructor is `HNilF`. Then we pattern match on a list of at least one
element, in which case the corresponding data-constructor is `HConsF`.

Constructing such a heterogeneous list works the same way as with the GADT
version.

``` haskell
-- type can be inferred
listF :: HListF '[Char, Bool]
listF = HConsF 'a' (HConsF True HNilF)
```

Recursion on the other hand is no longer as easy as with the GADT version. E.g.
the above version of `hLengthG` would not compile for `HListF`. Instead we have
to implement such functions through type-classes.

``` haskell
class HLengthF xs where
    hLengthF :: HListF xs -> Int

instance HLengthF '[] where
    hLengthF HNilF = 0

instance HLengthF xs => HLengthF (x ': xs) where
    hLengthF (HConsF x xs) = 1 + hLengthF xs
```

On the other hand, type inference works much better on the data family version.
The type of the example from before can now be inferred.

``` haskell
-- type can be inferred
mysteriousF :: HListF '[Char, Bool] -> (String, Bool)
mysteriousF = \(HConsF c (HConsF b HNilF)) -> (c:"", not b)
```

Therefore, when choosing how to implement heterogeneous lists we have to decide
on the following trade-off. Do we want more powerful type-inference on
expressions dealing with heterogeneous lists, or do we want recursive functions
on them without the need for type-classes. As we will see we will actually need
both. Fortunately, not at the same time.

## Heterogeneous Lists of Values in the Embedded Language

Let's first consider the kind of heterogeneous list that we expect the
functions in open recursion form to take. It would be very inconvenient to have
to add explicit type signatures whenever we define mutually recursive
functions. Therefore, we will side with the data family version. There is,
however, one more feature that we need to add. We expect that every element of
such a list is a value in the embedded language. I.e. every element must be of
the form `repr a`. To ensure that we implement heterogeneous lists in the
following way.

``` haskell
data family HList :: (* -> *) -> [*] -> *
data instance HList repr '[] = HNil
data instance HList repr (x ': xs) = repr x :. HList repr xs
infixr 5 :.
```

As its first argument the data family expects a higher kinded type. That will
be `repr`. Note, how we only apply `repr` to `x` on the right-hand side of the
equal sign. That means that `repr` will not be repeated in the type level list.
I.e. the following represents a list containing a `repr Int`, a `repr Bool`,
and a `repr (Int -> Bool)`.

``` haskell
-- type can be inferred (constraints omitted)
threeThings :: HList repr '[Int, Bool, (Int -> Bool)]
threeThings = int 4 :. bool True :. lam (\n -> n `eq` int 3) :. HNil
```

## Mutual Recursion Fixed-Point Combinator

The next question is how to actually use this data type for the mutual
recursion fixed-point combinator. Comparing to the homogeneous list
implementation from before we expect the mutual recursion fixed-point
combinator to (conceptionally) have the following type.

``` haskell
fix :: HList' repr xs -> HList repr ys
```

Each element of the argument list should be of the form `HList repr ys -> repr
y` and all those `y` together should form `ys`. The type `Hlist'` is yet to be
determined.

Unfortunately, this form would make it quite difficult to use mutually
recursive functions in the embedded language. As it stands the language has no
support for heterogeneous lists itself. I.e. we couldn't actually unpack the
result of `fix` within the embedded language. We could either take a detour
through Haskell-land to unpack the results of `fix` and then disperse them over
expressions in the embedded language, or we could add heterogeneous lists to
the language.

We will choose a third option: Instead of implementing a mutual recursion
fixed-point combinator as a primitive we will actually do the same thing that
Haskell does and provide mutually recursive let-bindings.

## Let-Bindings

So, what is a let-binding? In Haskell we all know what it is.

``` haskell
let answer = 42
 in "The answer to life, the universe, and everything is " ++ show answer
```

What that really means is that we bind some expression (`42`) to a name
(`answer`) and then make that name available in another expression. We've
actually seen that before. It is the same as defining a function of a parameter
(making the name available) and then calling it immediately with an argument
(binding some expression to that name). A simple non-recursive let-binding
could look as follows.

``` haskell
class Let1Lang repr where
    let1 :: repr a -> (repr a -> repr b) -> repr b

instance Let1Lang Eval where
    let1 x e = e x
```

A mutually recursive let-binding would then (conceptually) look like so.

``` haskell
class LetLang repr where
    let_ :: HList' repr xs -> (HList repr ys -> repr r) -> repr r
```

Where, again, each element of the first argument list should be of the form
`HList repr ys -> repr y` and all those `y` together should form `ys`. And,
again, the type `HList'` is yet to be determined. It is clear that `HList'`
will have to be some form of heterogeneous list. So, could we use a type
similar to `HList` for that purpose?

There are a number of complications: First, the mentioned constraints on the
arguments need to be enforced in some way. Second, remember one of the
restrictions of the data family implementation: To iterate over elements of a
data family implemented `HList` by recursion we would have to define a specific
type-class just for that purpose. But, the function `let_` is already within a
type-class. Particularly, within one that doesn't (and shouldn't) allow any
further constraints on `xs` or `ys` within an instance definition.

To address both those issues we will define a new, dedicated heterogeneous list
type for let-bindings. This time we use a GADT to enable recursion within an
implementation of `let_`.  Furthermore, we will define it in such a way that
the type constraints on `xs` and `ys` are guaranteed by the list's type itself.

``` haskell
data LetBindings' repr ys zs where
    End  :: LetBindings' repr ys '[] 
    (:&) :: (HList repr ys -> f z)
         -> LetBindings' repr ys zs
         -> LetBindings' repr ys (z ': zs)
infixr 5 :&

type LetBindings repr ys = LetBindings' repr ys ys
```

The type `LetBindings'` (with an apostrophe) takes a higher kinded type as its
first argument (`repr`) and then expects two type-lists `ys`, and `zs`.  It is
a list similar to `HList` in that it has a terminator `End`, and a cons `(:&)`.
However, the two type-lists allow us to enforce the required constraints on its
elements: The first one (`ys`) is meant to hold the complete list of
fixed-point types at all times. The second parameter is meant to be built up
with every element that we add to the list.  If we prepend an element of type
`HList repr '[Int, Bool] -> repr Bool` then the type `Bool` will be prepended
to the list `zs`.

The magic happens within the type-alias `LetBindings` (without apostrophe)
which enforces that both type-lists, `ys` and `zs`, are identical. This way it
is ensured that the whole list of open recursive functions will result in a
list of fixed-points that has the same type as the list that every element in
the list expects as a parameter. It is prohibited by the type-system to
construct an inconsistent list of let-bindings. E.g. we could *not* construct
the following.

``` haskell
wrong = check
    ( (\(anInt :. aBool :. HNil) -> anInt `eq` (int 0))
   :& (\(anInt :. aBool :. HNil) -> if_ aBool (int 1) (int 0))
   :& End )
  where
    -- used to enforce that ys == zs
    check :: LetBindings repr ys -> LetBindings repr ys
    check = id
```

Looking at what each of those functions returns we would expect the type-list
`zs` to be `'[Bool, Int]`. Looking at the argument lists and how these
arguments are used we would expect the type-list `ys` to be `'[Int, Bool]`. The
lists don't match and it would be impossible to express `wrong`'s type through
the type-alias `LetBindings`.  To enforce consistent let-bindings we define
`let_` in terms of this type-alias.

``` haskell
class LetLang repr where
    let_ :: LetBindings repr ys -> (HList repr ys -> repr r) -> repr r
```

Finally, we implement mutually-recursive let-bindings for the evaluator. An
implementation for the pretty-printer is available with the [example
codes](https://github.com/aherrmann/final-encoding-examples/blob/85d0b8f0dff6d8cb817743be3a05606b0a452712/src/Ex11_MutuallyRecursiveLetBindings.hs#L56).

``` haskell
instance LetLang Eval where
    let_ xs e = e (fx xs xs) where
        fx :: LetBindings Eval ys
           -> LetBindings' Eval ys ys'
           -> HList Eval ys'
        fx xs End = HNil
        fx xs (x :& xs') = x (fx xs xs) :. fx xs xs'
```

This implementation first finds all the fixed-points of the let-bindings and
then passes the results to the expression that was given as the second
argument. The fixed-points are determined by the mutual recursion fixed-point
combinator `fx`. That function iterates over all mutually recursive functions
given in open recursion form, calls them on the result of `fx`, and then sticks
them into the list of results. It might require some staring at it, but this
function's definition is conceptually identical to that of the homogeneous list
fixed-point combinator `fixL`. Note, that we have to pass the complete
`LetBindings` into the fixed-point combinator as well. Otherwise, the compiler
cannot infer that `ys` in `fx`'s signature is the same as the `ys` in `let_`'s
(implicit) signature.

## Using Mutually Recursive Let-Bindings

Finally, with mutually recursive let-bindings implemented in the embedded
language we can demonstrate a few use-cases.

We can implement the factorial function.

``` haskell
fac = let_ ( (\(self :. HNil) ->
               lam ( \n -> if_ (n `eq` int 0)
                               (int 1)
                               (n `mul` (ap self (n `sub` int 1)))) )
          :& End )
        ( \(fac :. HNil) -> fac )
```

``` haskell
>>> eval $ ap fac (int 4)
24
>>> putStrLn $ pretty fac
(let self0 = (\x0 -> if (x0 == 0) then 1 else (self0 (x0 - 1))); in self0)
```

We can implement the even and odd functions from before and use them to check
that no number is even and odd at the same time.

``` haskell
false = let_ ( (\(ev :. od :. HNil) ->
                 lam (\n -> (n `eq` int 0) `or`
                            ap od (n `sub` int 1)))
            :& (\(ev :. od :. HNil) ->
                 lam (\n -> (n `neq` int 0) `and`
                            ap ev (n `sub` int 1)))
            :& End )
          ( \(ev :. od :. HNil) ->
            lam (\n -> ap ev n `and` ap od n) )
```

``` haskell
>>> eval $ ap false (int 4)
False
>>> eval $ ap false (int 3)
False
```

We can actually turn full circle and implement the fixed-point combinator
within the embedded language.

``` haskell
fixE = let_ ( \(fix :. HNil) ->
              lam (\f -> ap f (ap fix f))
           :& End )
         ( \(fix :. HNil) -> fix )
```

``` haskell
>>> putStrLn $ pretty fixE
(let self0 = (\x1 -> (x1 (self0 x1))); in self0)
```

Which we can then use to implement the factorial function yet again.

``` haskell
fac2 = ap fixE $ lam (\fac ->
    lam (\n ->
      if_ (n `eq` (int 0))
          (int 1)
          (n `mul` ap fac (n `sub` int 1))
    ))
```

``` haskell
>>> eval $ ap fac2 (int 4)
24
```

## Conclusion and Outlook

In this article we found that it is indeed possible to implement mutually
recursive let-bindings for an embedded language in final encoding in a
type-safe and generic way. The given implementation can handle an arbitrary
number of bindings of differing types.

On a personal note, I can say that during this little project I was confronted
with many aspects of Haskell that were new to me. I was struggling quite a bit
with the subtle differences between GADTs and data families. It is very
surprising to see that while they both allow to implement type-safe
heterogeneous lists, they imply very different restrictions on the resulting
types.

For the particular use case here it would have been great to have a means of
constructing a heterogeneous list that does not hinder type-inference but at
the same time allows for recursive functions to iterate over elements without
the need for type-classes. If that is already possible in Haskell, today.
Please, let me know as I'm not aware of it.

One extension that I would love to add would be to write a variadic version of
`let_`. The implementation above requires a lot of boilerplate on the user's
side in terms of different list constructors and terminators. It would be much
nicer if the following interface was available in a generic way.  My (failed)
attempt at that is available with the [example
codes](https://github.com/aherrmann/final-encoding-examples/blob/85d0b8f0dff6d8cb817743be3a05606b0a452712/src/Ex12_VariadicLet.hs).

``` haskell
false = vlet ( \ev od ->
               lam (\n -> (n `eq` int 0) `or`
                          ap od (n `sub` int 1)) )
             ( \ev od ->
               lam (\n -> (n `neq` int 0) `and`
                          ap ev (n `sub` int 1)) )
             
       `in_` ( \ev od ->
               lam (\n -> ap ev n `and` ap od n) )
```

In any case, I hope you enjoyed this article. If you have any comments,
suggestions, or critique please leave a comment below.

Thank you for reading.
