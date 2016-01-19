---
layout: post
title: "Resource Management in Haskell"
description: "Resource management in Haskell from the perspective of a C++
              programmer"
category: programming
tags: [haskell]
---
{% include JB/setup %}

This is a post about Haskell. However, I would like to point out, that I am not
a professional Haskell programmer. I have never had the opportunity to use it
for production. However, I do enjoy using Haskell for side-projects.  So, if
you find that I made any mistake, please leave a comment.

I do most of my everyday programming in C++ or Python and this will be
reflected in this article. I will discuss resource management in Haskell from
the perspective of a C++ programmer.

I will show a number of example codes in this article. If you want to follow
along you can find the source code of all examples
[here](http://github.com/aherrmann/resource_management_in_haskell).

## Resource Management in C++

First I would like to revise resource management in C++. The recommended way to
manage resources in modern C++ is the
[RAII-idiom](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md#Rr-raii)
(Resource Acquisition Is Initialization). RAII might not carry the best name,
but it is a pattern that allows for code with no resource leaks, is exception
safe, and concise. It exploits the fact that objects are destroyed by the end
of their lifetime, and that the lifetime of an object local to a scope will end
when we leave that scope, independently of whether this happens due to an
exception or the normal course of the program. If we encapsulate resource
acquisition and release within a dedicated class and clearly define the
ownership then the compiler will do the rest for us.

Before we look at an example I would like to clarify what I mean by a resource.
Within this article it shall be something that has to be acquired in some way
before we can use it, and has to be released once we don't need it any longer.
If we forget to release the resource then we have a leak in our application.
Examples include allocated memory, file-handles, and locks on mutexes.

All of the above are difficult to observe. Therefore, we will emulate a more
verbose resource that notifies us whenever it is acquired or released.

{% highlight cpp %}
class Resource {
  public:
    Resource (std::string name) : name_(std::move(name)) {
        std::cout << "Acquired " << name_ << "\n";
    }
    ~Resource () {
        std::cout << "Released " << name_ << "\n";
    }

  private:
    std::string name_;
};
{% endhighlight %}

This defines the class `Resource`. Its constructor takes a string parameter, a
name by which we want to refer to the particular instance. Both, the
constructor and destructor write the name to `stdout` such that we can trace
the resource's lifetime.  The following code demonstrates how to use this
class.

{% highlight cpp %}
int main() {
    try {
        Resource a("A");
        Resource b("B");
        // do something with a and b
        throw std::exception();
        // do some more with a and b
    } catch (const std::exception &) {
        std::cout << "Oops\n";
    }
}
{% endhighlight %}

Here we acquire two resources `"A"`, and `"B"`, and then do some work with
them. However, something goes wrong in the middle of it and we end up throwing
an exception. The program creates the following output.

```
Acquired A
Acquired B
Released B
Released A
Ooops
```

As we can see, both resources are released before we handle the exception. The
compiler inserted all the necessary exception handling code for us. The
exception is passed on after the resources are released.

## Resource Management in Haskell

In comparison to that we will look at how resource management can be done in
Haskell.  If we go through the [documentation of the module
`System.IO`](http://hackage.haskell.org/package/base-4.8.1.0/docs/System-IO.html#g:4)
in the Haskell package `base` we find functions such as `openFile`, and
`hClose` which can be used for manual resource handling, much like `fopen` and
`fclose` in C.  However, we also find functions such as `withFile` which ensure
that the resource will be released after we don't need it any longer, much like
the C++ resource class above.

For the same reason as before we will first define a Haskell version of our
emulated verbose resource and the corresponding pair of open and close
functions.

{% highlight haskell %}
data Resource = Resource String

acquireResource :: String -> IO Resource
acquireResource name = do
  putStrLn $ "Acquired " ++ name
  return $ Resource name

releaseResource :: Resource -> IO ()
releaseResource (Resource name) = putStrLn $ "Released " ++ name
{% endhighlight %}

To demonstrate exception handling we define the following exception type.

{% highlight haskell %}
data ResourceException = ResourceException deriving (Show, Typeable)
instance Exception ResourceException
{% endhighlight %}

Our newly defined resource can be used in the following way.

{% highlight haskell %}
main :: IO ()
main = handle (\ResourceException -> putStrLn "Oops") $ do
  a <- acquireResource "A"
  b <- acquireResource "B"
  -- do something with a and b
  throwIO ResourceException
  -- do some more with a and b
  releaseResource b
  releaseResource a
{% endhighlight %}

However, when we look at the output we find that there is a problem.

```
Acquired A
Acquired B
Oops
```

The resources have not been released at all. To fix this problem we have to
define our own `withResource` function. We need a function that first acquires
the resource, then performs some user defined action on it, and makes sure that
the resource will be released in the end regardless of whether an exception was
thrown or not. Luckily, Haskell already provides us with the [function
`bracket`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception-Base.html#v:bracket)
in the module `Control.Exception` which does exactly what we want. Its first
argument defines how to acquire the resource, its second argument how to
release the resource, and the third argument how to use the resource.  With its
help we can define `withResource`.

{% highlight haskell %}
withResource :: String -> (Resource -> IO r) -> IO r
withResource name = bracket (acquireResource name) releaseResource
{% endhighlight %}

An exception safe version of the above example can be implemented as follows.

{% highlight haskell %}
main :: IO ()
main = handle (\ResourceException -> putStrLn "Oops") $
  withResource "A" $ \a ->
    withResource "B" $ \b -> do
      -- do something with a and b
      throwIO ResourceException
      -- do some more with a and b
{% endhighlight %}

And it produces the expected output.

```
Acquired A
Acquired B
Released B
Released A
Oops
```

This fixed the problem and made our code exception safe. However, we have to
break out of do-notation for the resource management and the names that we bind
our resources to are hidden in the syntactic noise of lambda definitions. This
might be just a question of aesthetics, however if we add a few more resources
and some productive code this could quickly become difficult to read.

Being still relatively new to Haskell I started to wonder whether there was
some way to define a scope-monad where we could acquire a resource within the
same block of do notation but at the same time ensure that the resource will be
released in the end in an exception safe way. Naively I started out with a
state-monad that held a list of resources that would automatically be released
in the end. However, I quickly found that the implementation became very
complicated when taking exception-safety, nested scopes, and stacks of
monad-transformers into account. Furthermore, we would be giving up on the full
wealth of `with...` functions that Haskell already has to offer.

I took a step back and had a closer look at these `with...` functions. In the
end they all boil down to the following signature `(a -> IO r) -> IO r`, i.e.
functions that take a function `a -> IO r`, apply it to the handled resource
`a`, and finally return the result of the computation `IO r`. In other words,
the `with...` functions don't return the resource as a result, but rather pass
it on to another function. This pattern is called [*continuation passing
style*](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style) and
it can be represented by a
[monad](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style#The_Cont_monad).
The transformers package contains a [monad transformer for the continuation
monad](http://hackage.haskell.org/package/transformers-0.5.0.0/docs/Control-Monad-Trans-Cont.html#v:ContT)
and its constructor's signature matches that of the `with...` functions
perfectly `ContT :: (a -> m r) -> m r -> ContT r m a`.  With it we can
transform our previous example into the following form.

{% highlight haskell %}
main :: IO ()
main = handle (\ResourceException -> putStrLn "Oops") $ evalContT $ do
  a <- ContT $ withResource "A"
  b <- ContT $ withResource "B"
  -- do something with a and b
  liftIO $ throwIO ResourceException
  -- do some more with a and b
{% endhighlight %}

In the continuation monad we can build up a chain of continuations.  However,
in the end we will have to decide on a final continuation and return a result.
In our case the final continuation should just return the result of whatever we
were doing in the `IO` monad. The function `evalContT` with the signature
`ContT r m r -> m r` does that for us. It passes `return` as the final
continuation and performs the whole computation.  Thanks to the `ContT` monad
transformer we can safely handle our resources within the same do block. Note,
that we have to lift `IO` actions into the continuation monad using `liftIO`.

The above code produces the expected output, the same as that of the previous
example. That is good news. We found that the scope-monad that we were looking
for is just a special case of the continuation monad. We get all the benefits
of a monad, such as do notation, and, best of all, we didn't actually have to
do anything. All the required functionality was already there.

## One Step Further

In C++ the RAII idiom has been used for more than just resource management. One
example are *scope guards*, which define actions that should be performed by
the very end of the scope. Alexei Alexandrescu has
[defined](http://www.drdobbs.com/cpp/generic-change-the-way-you-write-excepti/184403758?pgno=3)
and later [refined](http://www.youtube.com/watch?v=WjTrfoiB0MQ) the notion for
C++.  For this article we will distinguish three cases: `scopeExit`,
`scopeFail`, `scopeSuccess`. As it turns out, all these are actually very easy
to implement in the scope-monad.

We will start with `scopeExit` which is supposed to always execute the given
action by the end of the scope. Haskell already has a function for this task.
It is called `finally` and part of the module `Control.Exception`. However, its
signature `IO a -> IO b -> IO a` is not compatible with `ContT`. Therefore, we
have to wrap it in the following way.

{% highlight haskell %}
scopeExit :: IO a -> ContT r IO ()
scopeExit action = ContT $ \f -> f () `finally` action
{% endhighlight %}

This means that the result of `scopeExit` expects a continuation with the
signature `() -> IO r`. In other words, `scopeExit` has nothing to pass on to
its continuation.

Next we want to implement `scopeFail`. It is supposed to only execute the given
action if an exception was thrown.  As before we can implement it using a
library function, namely `onException`. Its signature is identical to that of
`finally` and we have to wrap it in the same way.

{% highlight haskell %}
scopeFail :: IO a -> ContT r IO ()
scopeFail action = ContT $ \f -> f () `onException` action
{% endhighlight %}

Finally, `scopeSuccess` is a little harder to implement as there is no library
function in Haskell which already does what we want. It is supposed to only
execute the given action if no error occurred. However, if we lookup the
implementation of `finally` we find that we only have to modify it slightly.

{% highlight haskell %}
scopeSuccess :: IO a -> ContT r IO ()
scopeSuccess action = ContT $ \f -> do
  mask $ \restore -> do
    r <- restore (f ())
    _ <- action
    return r
{% endhighlight %}

Here we use the [function
`mask`](http://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Exception-Base.html#v:mask),
which takes a function in the `IO` monad as its argument and masks all
asynchronous exceptions during its execution.  However, it passes another
function to its argument that can be used to restore the outer masking state.
In the above example that means, that `f ()` could be interrupted by an
asynchronous exception, but `action` will be executed with asynchronous
exceptions masked, i.e. a throwing thread will be blocked until asynchronous
exceptions are unmasked again. If an exception is thrown during the execution
of `f ()` then `action` will never be executed.

The following code demonstrates how these three functions can be used.

{% highlight haskell %}
demo :: Bool -> IO ()
demo throw = evalContT $ do
  scopeExit $ putStrLn "Leaving scope"
  scopeFail $ putStrLn "Scope failed"
  scopeSuccess $ putStrLn "Scope succeeded"
  liftIO $ putStrLn "Inside scope"
  when throw $ liftIO $ throwIO ResourceException
  liftIO $ putStrLn "Did we just throw?"

main :: IO ()
main = do
  handle (\ResourceException -> putStrLn "Oops") $ demo True
  putStrLn $ replicate 50 '-'
  handle (\ResourceException -> putStrLn "Oops") $ demo False
{% endhighlight %}

The function `demo` takes a Boolean parameter that determines whether an
exception will be thrown. It then establishes three scope guards, does some IO,
and optionally throws an exception. The program's output looks as follows.

```
Inside scope
Scope failed
Leaving scope
Oops
--------------------------------------------------
Inside scope
Did we just throw?
Scope succeeded
Leaving scope
```

In the first case an exception was thrown and `scopeFail`, and `scopeExit`
executed their actions. In the second case no exception was thrown and
`scopeSuccess`, and `scopeExit` executed their actions. This means that we
achieved our goal and implemented all three scope guards in Haskell. Note, that
it is possible for exceptions to be thrown within the scope guards. If, in the
above example, an exception is thrown within `scopeSuccess`'s action, then
`scopeFail` will also execute its action. This means that the order in which
scope guards are defined matters.  Also note, that `scopeSuccess` is only
necessary if you want to make sure that the embedded action will not be
interrupted by asynchronous exceptions. Otherwise, you could just place the
embedded action at the very end of the do notation block.

## Practical Application

Finally, I would like to give a practical example of what we have learned.
Suppose we are given the task to write a program that copies a large file and
prints its progress in percent on standard output so that it can be piped into
a tool such as [dialog](http://invisible-island.net/dialog/) or
[Zenity](https://wiki.gnome.org/Projects/Zenity). In order to do this we will
need a number of resources. First, we will need file-handles to the source and
destination files, and second, we will need a buffer to read into and write
from. This makes it a good example to show off the scope-monad that we defined
above.

First we include all of the required modules.

{% highlight haskell %}
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Cont (ContT (..), evalContT)
import Foreign.Marshal.Alloc (allocaBytes)
import System.IO
{% endhighlight %}

We define the buffer size as a global constant. In this case we will use 1 MiB.

{% highlight haskell %}
bufferSize :: Int
bufferSize = 1024 * 1024
{% endhighlight %}

Since we want to print the progress in percent we need a way to calculate
these numbers.

{% highlight haskell %}
percentOf :: Integral a => a -> a -> a
percentOf part all = (part * 100) `div` all
{% endhighlight %}

Finally, we define the main program.

{% highlight haskell %}
main :: IO ()
main = evalContT $ do
  infile <- ContT $ withBinaryFile "infile" ReadMode
  outfile <- ContT $ withBinaryFile "outfile" WriteMode
  buffer <- ContT $ allocaBytes $ bufferSize
  liftIO $ hSetBuffering infile NoBuffering
  liftIO $ hSetBuffering outfile NoBuffering
  fileSize <- liftIO $ hFileSize infile
  let copy progress = do
        print $ progress `percentOf` fileSize
        bytesRead <- hGetBuf infile buffer bufferSize
        hPutBuf outfile buffer bytesRead
        unless (bytesRead == 0) $ copy (progress + fromIntegral bytesRead)
  liftIO $ copy 0
{% endhighlight %}

We first open the input and output files in binary mode. Then we allocate
memory for the buffer. All these resources are acquired in the continuation
monad and will be released in the end even if an exception occurs. Next we
deactivate buffering on both file handles. We don't need it, since we already
define our own buffer. Then we measure the size of the input file which we need
to calculate the progress. Finally we start copying in a loop.  Each iteration
prints the current progress in percent and then copies data from the input file
to the output file. We leave the loop when the end of the input file is reached
and `hGetBuf` does not read any further bytes of data. The full code is
available
[here](https://github.com/aherrmann/resource_management_in_haskell/blob/master/06_haskell_copy_cont.hs).

## Conclusion

With this I will conclude this already rather lengthy article. We compared
resource management in C++ and in Haskell and found a monad in which we could
embed the resource handling in an exception safe way. In the end a surprisingly
little amount of work was required to achieve this goal. Due to the use of the
continuation monad transformer we can keep using all the existing `with...`
functions. Furthermore, the solution extends beyond the `IO` monad. `ContT` is
a general monad transformer and we could use it to define a scope in any other
monad. A somewhat contrived example would be actions on a stack that is
represented by a `State [Int]` monad. The source code for such an example is
available
[here](https://github.com/aherrmann/resource_management_in_haskell/blob/master/07_haskell_stack.hs).

I was surprised to see that I could not find any mention of the continuation
monad being used in this way anywhere on the internet, or in its documentation.
I will readily admit that before I found this application I was never quite
sure what practical use it might have. I hope that I was able to demonstrate a
practical use case for the continuation monad, and I hope it can be of use to
others. Let me point out, that I am not suggesting to use the continuation
monad in every case where you need to handle resources. However, if you find
that nested `with...` functions stack up to an uncomfortable depth, then the
continuation monad might be a good solution.

Any criticism, suggestion, or any other form of feedback will be appreciated.
So, please feel invited to leave a comment below. Thanks for reading!

## Update

It was pointed out to me that the package
[managed](https://hackage.haskell.org/package/managed) provides a specialized
monad for resource management based on the continuation monad. Furthermore, the
package [resourcet](https://hackage.haskell.org/package/resourcet) implements a
monad for resource management with additional features such as controlled
release of a resource at any point.
