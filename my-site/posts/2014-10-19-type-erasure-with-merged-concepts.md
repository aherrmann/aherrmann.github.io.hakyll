---
layout: post
title: "Type Erasure with Merged Concepts"
category: programming
tags: [C++]
---
{% include JB/setup %}

Two days ago, I watched a very interesting talk by Zach Laine: [Pragmatic Type
Erasure: Solving OOP Problems with an Elegant Design
Pattern](https://www.youtube.com/watch?v=0I0FD3N5cgM). The question that Zach
Laine addresses is how to provide polymorphic interfaces while, at the same
time, adhering to value semantics. I do also recommend the following talk by
Sean Parent, which gives a great introduction into the concept and the benefits
of type-erasure, and value semantics: [Inheritance Is The Base Class of
Evil](https://www.youtube.com/watch?v=bIhUE5uUFOA)

This post is motivated by a question that came up on
[Reddit](http://www.reddit.com/r/cpp/comments/2jb295/cppcon_2014_pragmatic_type_erasure_solving_oop/clceg2c).
Namely, how can we merge multiple type-erased interfaces into one single
interface. A similar question is also asked in the end of the first talk: How
to apply type erasure to types with overlapping interfaces? The speaker's
answer is to simply repeat the common parts. I think there has to be a better
way. So, in this post I am going to explore how to merge type-erased
interfaces. But first, let's quickly revise type-erasure.

(Note, the C++ code examples are simplified in favour of readability. A link to
working code is provided at the end of each section.)

## Type Erasure

Suppose we want to write a function which greets a person named Tom in some
way. I.e. could print "Hi Tom", "Hello Tom", "Good day Tom", ... you get the
idea. The function should accept an argument that specifies how to greet a
person. We will call this argument a *Greeter*.  Here is a simple
implementation of our function:

{% highlight cpp %}
void greet_tom(const Greeter &g) {
    g.greet("Tom");
}
{% endhighlight %}

A user of this function may now wish to greet Tom in English and in French.
So, he implements two Greeters:

{% highlight cpp %}
struct English {
    void greet(const std::string &name) const {
        std::cout << "Good day " << name << ". How are you?\n";
    }
};

struct French {
    void greet(const std::string &name) const {
        std::cout << "Bonjour " << name << ". Comment ca va?\n";
    }
};
{% endhighlight %}

Now, how can the user pass his Greeters to our function?  Classically, we could
either define an abstract base class and let our user derive from it, or we
could make `greet_tom` a function template in `Greeter`. Both methods have
their down-sides, which are described in the above mentioned talks.

With type-erasure, we will hide the templates, and the inheritance under the
covers. We will define a `Greeter` class that can be initialized with anything
that provides the expected Greeter interface. Following Sean Parent's pattern
an implementation could look as follows:
{% highlight cpp %}
class Greeter {
  public:
    // Constructor: We can stuff anything into a Greeter costume.
    template <class T>
    Greeter(T data) : self_(std::make_shared<Model<T>>(data)) {}

    // External interface: Just forward the call to the wrapped object.
    void greet(const std::string &name) const {
        self_->greet(name);
    }

  private:
    // The abstract base class is hidden under the covers...
    struct Concept {
        virtual ~Concept() = default;
        virtual void greet(const std::string &) const = 0;
    };
    // ... and so are the templates.
    template <class T>
    class Model : public Concept {
      public:
        Model(T data) : data_(data) {}
        virtual void greet(const std::string &name) const override {
            // Forward call to user type.
            // Requires that T can greet.
            data_.greet(name);
        }

      private:
        // The user defined Greeter will be stored here. (by value!)
        T data_;
    };

    // Polymorphic types require dynamic storage.
    // Here we store our pointer to the Model that holds the users Greeter.
    std::shared_ptr<const Concept> self_;
};
{% endhighlight %}

Note that we are using a shared-pointer to *const* to refer to the
implementation. The details are explained in Sean Parent's talk. We get
copy-on-write and value-semantics out of it for free (Magic!). I chose it here,
because it eliminates all the boiler-plate for copy/move
construction/assignment.

A working example of the code is available
[here](http://coliru.stacked-crooked.com/a/435fa7cefc0a988d).


## Multiple Concepts

The problem arises when we want to merge two existing interfaces. For example,
suppose there is a second concept: A door-opener, short Opener. I.e. a thing
that opens doors. In some places of our code an Opener will be sufficient, in
some other places we only need a Greeter, but in some places we need to first
open the door for someone and then greet them:

{% highlight cpp %}
void open_door_and_greet_john(const OpenerAndGreeter &g) {
    g.open();
    g.greet("John");
}
{% endhighlight %}

How do we create `OpenerAndGreeter`? Well, we can just create a whole new
class for it and copy-paste the Opener, and Greeter parts into it. Like so:

{% highlight cpp %}
class OpenerAndGreeter {
  public:
    template <class T>
    OpenerAndGreeter(T data) : self_(std::make_shared<Model<T>>(data)) {}

    void open() const { self_->open(); }
    void greet(const std::string &name) const { self_->greet(name); }

  private:
    struct Concept {
        virtual ~Concept() = default;
        virtual void open() const = 0;
        virtual void greet(const std::string &) const = 0;
    };
    template <class T>
    class Model : public Concept {
      public:
        Model(T data) : data_(data) {}
        virtual void open() const override { data_.open(); }
        virtual void greet(const std::string &name) const override {
            data_.greet(name);
        }

      private:
        T data_;
    };

    std::shared_ptr<const Concept> self_;
};
{% endhighlight %}

But this is not ideal. It would be much better if we could take an existing
Greeter concept, and an existing Opener concept, and just merge the two
together.

A working example of the code is available
[here](http://coliru.stacked-crooked.com/a/1db9816bbf22b3c0).


## Dissecting Type-Erasure

Before we get there we need to understand what our type-erasure class
actually does. So let's take the Greeter apart.

First, it defines an abstract base class Concept. This is very specific to
the Greeter. But, it has nothing to do with type-erasure. So, we pull it out.

{% highlight cpp %}
// Defines the concept of a Greeter.
struct Concept {
    virtual ~Concept() = default;
    virtual void greet(const std::string &name) const = 0;
};
{% endhighlight %}

Second, there is the model of that concept. This actually does two things: It
holds an arbitrary value, and it passes the concept's interface through to
that value. So, let's separate them.
{% highlight cpp %}
// Holds a value of arbitrary type.
template <class T>
class Holder {
  public:
    Holder(T obj) : data_(std::move(obj)) {}
    virtual ~Holder() = default;
    const T &get() const { return data_; }

  private:
    T data_;
};

// Passes the Concept's interface through to the held value.
template <class Holder>
struct Model : public Holder, public Concept {
    using Holder::Holder;  // pull in holder's constructor
    virtual void greet(const std::string &name) const override {
        this->Holder::get().greet(name);
    }
};
{% endhighlight %}

Next, Greeter is also a container that refers to a concept, and initializes it
with a model. This is very specific to type-erasure, but has nothing to do
with greeting people.

{% highlight cpp %}
template <class Concept, template <class> class Model>
class Container {
  public:
    template <class T>
    Container(T obj)
        : self_(std::make_shared<Model<Holder<T>>>(std::move(obj))) {};

    const Concept &get() const { return *self_.get(); }

  private:
    std::shared_ptr<const Concept> self_;
};
{% endhighlight %}

And after all this hacking and slashing there is only one bit left. Namely,
the external interface that passes calls through to the container.

{% highlight cpp %}
template <class Container>
struct ExternalInterface : public Container {
    using Container::Container;  // pull in container's constructor
    void greet(const std::string &name) const {
        this->Container::get().greet(name);
    }
};
{% endhighlight %}

Great! We started out with a perfectly well functioning class and took it apart
into tiny pieces. Now we need to reassemble them and make sure that it still
works.  But don't forget, the goal of this exercise is to make concepts
mergeable — automatically. Hence, we need an automated way to assemble all the
pieces that we created. So, it's time for some template magic.


## Automated Type-Erasure

Above pieces fall into two categories: One, there are pieces that define the
Greeter's interface, and two, there are pieces which define how to hold and
call objects of arbitrary types. On the holding and calling side we find
`Holder`, and `Container`, which are implementation details of our
type-erasure container; whereas `Concept`, `Model`, and `ExternalInterface`
are details of a Greeter. To keep things in order we will collect the Greeter
parts in a super type that we call `GreeterSpec`.

At this stage we can write a template class that assembles all these pieces
together and constructs a type-erasure container for an arbitrary spec. It
will take the spec's `ExternalInterface` template, and instantiate it with a
container for the spec's concept, and model. It will also pull in the
base-classes constructor, so that we can still construct it from objects of
arbitrary types.

{% highlight cpp %}
template <class Spec>
struct TypeErasure
    : public Spec::ExternalInterface<Container<Spec::Concept, Spec::Model>> {
    using Base =
        Spec::ExternalInterface<Container<Spec::Concept, Spec::Model>>;
    using Base::Base;
};

using Greeter = TypeErasure<GreeterSpec>;
{% endhighlight %}

As the last line demonstrates, the Greeter itself is nothing but a TypeErasure
of a certain spec.

Again, a working example of the code is available
[here](http://coliru.stacked-crooked.com/a/f68c2c4f1c106d60).


## Merging Concepts

Now, with all that machinery backing us, we can tackle the original problem:
How to merge two concepts? We have a tool that creates a type-erasure class
out of an arbitrary spec. And, we assume that we already have a `GreeterSpec`,
and an `OpenerSpec` that define those two concepts. What we need is a tool to
automatically merge two specs into one. Let's approach this component by
component.

How do we merge `Concept` classes, i.e. interfaces? In C++ we do this by
multiple inheritance:

{% highlight cpp %}
struct Concept : public virtual ConceptA, public virtual ConceptB {};
{% endhighlight %}

How about the Models? The model is a template class that takes a holder as a
template parameter and then inherits from said holder, thus becoming a holder
itself. So, we can take `SpecB`, and the holder, and merge them into one class.
This new class will itself be a holder. Next, we take `SpecA`, and that new
holder, and merge them to get our final merged Model. There is one nifty
detail, though: We need to use virtual inheritance for the concepts. The
reason is that `ConceptA`, and `ConceptB` will enter the merged `Model`
through the merged `Concept`, but also through the models of the two specs.

{% highlight cpp %}
template <class Holder>
struct Model : public SpecA::Model<SpecB::Model<Holder>>,
               public virtual Concept { /* ... */ };
{% endhighlight %}

The external interfaces are merged the same way, just without the concepts:

{% highlight cpp %}
template <class Container>
struct ExternalInterface
    : public SpecA::ExternalInterface<SpecB::ExternalInterface<Container>> {
    /* ... */
};
{% endhighlight %}

Finally, to construct a merged spec we take all the above items and wrap them
in a template class, that takes two specs:

{% highlight cpp %}
template <class SpecA, class SpecB>
struct MergeSpecs {
    /* ... */
};
{% endhighlight %}

With this it is trivial to create a type-erasure that merges two concepts:
{% highlight cpp %}
using OpenerAndGreeter = TypeErasure<MergeSpecs<OpenerSpec, GreeterSpec>>;
{% endhighlight %}

And with just a little bit more of template magic it is even possible to merge
two existing type-erasure classes. So, with all the above we write the
following code:

{% highlight cpp %}
using Opener = TypeErasure<OpenerSpec>;
using Greeter = TypeErasure<GreeterSpec>;
using OpenerAndGreeter = MergeConcepts<Opener, Greeter>;
{% endhighlight %}

Done!

And this last code example is available
[here](http://coliru.stacked-crooked.com/a/08ad06b59f98bf25).


## Conclusion & Outlook

We find that it is indeed possible to merge two existing type-erasure classes
into one that has a common interface. And what's more, we can do it fully
automatically and in just one line of code. The costly bit is to define the
original type-erasure classes. For each one we need to define a spec class, and
manually define the interface. The reason is that C++ does not support
introspection. On the other hand, these specs follow a fairly strict scheme and
it should be quite possible to produce them through tooling, or possibly even a
macro.

Another possible issue is the inheritance pattern for `Model`, and
`ExternalInterface`. Due to the chaining of base classes we introduce user
specified names into the classes `Holder`, and `Container`. The method `get`
could be shadowed by a user method. The library code does actually contain more
template magic to avoid this problem. A template meta-function peels layers of
derived classes off until it arrives at the actual `Holder`, or `Container`
class. An external getter function is provided for the user, which makes sure
to call the correct getter method. An obscured name of the internal getter
method provides further protection.

The full code is available [here](https://github.com/aherrmann/rubber_types).
Please feel invited to try it out and give me your feedback. Also, since this
is my first blog post, any criticism is very welcome.

Thanks for reading!


## Comments

Unfortunately, I have not yet figured out how to add comments to github pages.
For the moment I would like to defer any discussion to
[Reddit](http://www.reddit.com/r/cpp/comments/2jp167/type_erasure_with_merged_concepts/).
I apologize for the inconvenience.
