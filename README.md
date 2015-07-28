# derivation-hook: minimal dependency type class instance derivation support

This project is a proof of concept of minimal infrastructure to support non-orphan type class derivation without
imposing heavyweight dependencies on type class providers.

## What are "orphan" type class instances?

An _orphan_ type class instance is a type class instance which is defined outside its _implicit scope_. To understand
this we first need to review a few details of how the scala compiler resolves implicit parameters ...

Implicit definitions are resolved as implicit parameters in two ways,

1. a matching definition in the current or an enclosing scope or via an import.
2. a matching definition in the _implicit scope_.

The _implicit scope_ of a type `T` consists of all companion objects of the type that are associated with it. To a
first approximation the types associated with `T` are all the types that are mentioned in `T`. So, for example, the
implicit scope of `Functor[List]` includes the companion object of `Functor` and the companion object of `List`. The
full rules are more complicated and can be found in [section 7.2][sls-7.2] of the Scala Language Reference.

When the compiler is resolving an implicit parameter it will first look for defintions using strategy (1) above:
implicit defintions which are directly accessible in the current or an enclosing scope, or via an import, will be
consulted first and selected according to the [normal overload resolution rules][sls-6.26.3].

The compiler will _only_ use strategy (2), searching the implicit scope, if no suitable implicit definitions are found
using strategy (1). Or, to put it another way, directly accessible implicit definitions will always trump implicit
definitions in the implicit scope. [Eugene Yokota][eed3si9n] has an excellent writeup of the mechanics
[here][import-tax].

As a general rule we prefer implicit definitions to be available without needing explicit imports so, by and large,
it's preferable to define type class instances in the implicit scope, ie. within the companion object of the type
class trait, or within the companion object of the type for which we're requesting the instance.

For example, given the `Functor` type class and some type `Foo`, we would prefer to define the instance `Functor[Foo]`
in either the companion object of `Functor` or the companion object of `Foo`. In this case, whenever `Functor` and
`Foo` are visible, `Functor[Foo]` can be implicitly resolved without any further imports.

Conversely, an implicit defintion which is defined outside its implicit scope will only be resolved as an implicit
parameter if it is directly accessible, ie. if it is imported or defined in the current or an enclosing scope. This
would be the case if the instance `Functor[Foo]` were provided by a third-party, neither in the companion object of
`Functor` nor in the companion object of `Foo`. Type class instances defined in this way are _orphans_.

## Why are orphans a problem for automatic type class derivation?

Pulling the above together we can conclude,

+ Orphan type class instances must be imported (because they're not in the implicit scope).
+ Orphan type class instances will trump non-orphan type class instances (because directly accessible implicits are
  found during the first phase of implicit resolution before the implicit scope is consulted).

For many purposes these are exacly the semantics we want: orphan type class instances are often used locally to
provide an alternative to the default instance of a type class which has been provided for a given type. In this
scenario we want the directly accessible local definition to trump the definitions in the implicit scope.

However, these aren't the semantics we want for automatically derived type class instances. Here we want hand written
instances, which will typically be defined in the implicit scope, to take precedence over derived instances. This
directly conflicts with the fact that orphans must be imported.

To complicate matters still further it's common for there to be a default type class instance for a very general type
such as `AnyRef` or `Any`. This will typically be defined as a lowest priority instance in the companion object of the
type class trait. We will want derived instances to have a higher priority than these very general instances.

In other words, we want derived instances to have a priority inbetween hand-crafted specific instances (in the
implicit scope) and very general fallback instances (also in the implicit scope). This is very difficult to achieve
with orphan instances.

## Dealing with derived orphans

[shapeless][shapeless] has an evolving set of mechanisms which help to deal with this problem, but it would be even
better if we didn't have to deal with this problem at all. One way achieve this would be for projects which provide
type classes to include the derivation mechanism directly. Whilst this has some benefits, it forces a dependency on
shapeless on those projects, which might be too heavyweight for them, and which might limit shapeless ability to
evolve quickly.

This project is a proof of concept of an alternative mechanism, a _derivation hook_, which allows derived type class
instances to be inserted into the implicit scope with the appropriate priority _without_ requiring a shapeless
dependency. Instead the only dependency would be this project, which currently defines only the following two trivial
traits,

```scala
trait Deriver0[F[_], T] {
  def derive: F[T]
}

trait Deriver1[F[_[_]], T[_]] {
  def derive: F[T]
}
```

Additionally, the project providing type classes, and other parties, would have to follow the following conventions
...

## As seen by the type class provider

The cooperating type class provider has to include a hook for a type class deriver. That implies a dependency on this
project, and a hook trait as part of the stack of prioritized companion object traits,

```scala
package tc

import derivation.Deriver0

trait Tc[T] {
  // Type class defns ...
}

object Tc extends TcDerived {
  // Instances which should be higher priority than derived
  // instances should be defined here ...
}

trait TcDerived extends TcLowPriority {
  // Hook for a type class deriver ...
  implicit def derive[T](implicit deriver: Deriver0[Tc, T]): Tc[T] = deriver.derive
}

trait TcLowPriority {
  // Instances which should be lower priority than derived
  // instances should be defined here ...
}
```

## As seen by the type class deriver

The type class deriver has to provide a deriver instance which is able to provide instances of the requested types.
They can do that using shapeless or any other suitable mechanism,

```scala
package tcderiver

import derivation._, tc._, shapeless._

// Provide a deriver instance for Tc[T] for all T with a shapeless Generic instance

implicit def apply[T](implicit wtct: DerivedTc[T]): Deriver0[Tc, T] =
  new Deriver0[Tc, T] {
    def derive: Tc[T] = wtct
  }

trait DerivedTc[T] extends Tc[T]

object DerivedTc {
  implicit def hnil: DerivedTc[HNil] = ...

  implicit def hcons[H, T <: HList]
    (implicit hd: Tc[H], tl: Lazy[DerivedTc[T]]): DerivedTc[H :: T] = ...

  implicit def cnil: DerivedTc[CNil] = ...

  implicit def ccons[H, T <: Coproduct]
    (implicit hd: Tc[H], tl: Lazy[DerivedTc[T]]): DerivedTc[H :+: T] = ...

  implicit def gen[T, R]
    (implicit gen: Generic.Aux[T, R], mtcr: DerivedTc[R]): DerivedTc[T] = ...
}

```

## As seen by the type class user

The type class user should import both the type class and the type class deriver.

```scala
package tcuser

import tc._, tcderiver._

case class Foo(i: Int, s: String)

implicitly[Tc[Foo]]
```

If the type class user doesn't want to depend on any type class derivation mechanism they don't have to, in which case
they will only see underived instances.

## Feedback wanted!

This is a proof of concept ... please create issues here or hop on the [shapeless][shapeless-gitter] or
[cats][cats-gitter] gitter channels and let us know what you think.

[sls-7.2]: http://scala-lang.org/files/archive/spec/2.11/07-implicits.html#implicit-parameters
[sls-6.26.3]: http://scala-lang.org/files/archive/spec/2.11/06-expressions.html#overloading-resolution
[eed3si9n]: https://twitter.com/eed3si9n
[import-tax]: http://eed3si9n.com/revisiting-implicits-without-import-tax
[shapeless]: https://github.com/milessabin/shapeless
[shapeless-gitter]: https://gitter.im/milessabin/shapeless
[cats-gitter]: https://gitter.im/non/cats
