# export-hook: minimal dependency support for expanding type class implicit scope

This project is a proof of concept of minimal infrastructure to support the inclusion of derived and subclass
instances in the implicit scope of a type class without imposing heavyweight dependencies on the type class provider.

## What are "orphan" type class instances?

An _orphan_ type class instance is a type class instance which is defined outside its _implicit scope_. To understand
this we first need to review a few details of how the scala compiler resolves implicit parameters ...

Implicit definitions are resolved to satisfy implicit parameters in two ways,

1. by searching for a matching definition in the current or an enclosing scope or via an import.
2. by searching for a matching definition in the _implicit scope_.

The _implicit scope_ of a type `T` consists of all companion objects of the types that are associated with it. To a
first approximation the types associated with `T` are all the types that are mentioned in `T`. So, for example, the
implicit scope of `Functor[List]` includes the companion object of `Functor` and the companion object of `List`. The
full set of rules is more complicated and can be found in [section 7.2][sls-7.2] of the Scala Language Reference.

When the compiler is resolving an implicit parameter it will first look for defintions using strategy (1) above:
implicit defintions which are directly accessible in the current or an enclosing scope, or via an import will be
consulted first and selected according to the [normal overload resolution rules][sls-6.26.3].

The compiler will _only_ use strategy (2), searching the implicit scope, if no suitable implicit definitions are found
using strategy (1). Or, to put it another way, directly accessible implicit definitions will always trump implicit
definitions in the implicit scope. [Eugene Yokota][eed3si9n] has an excellent writeup of the mechanics
[here][import-tax].

As a general rule we prefer implicit definitions to be available without needing explicit imports so, by and large,
it's preferable to define type class instances in the implicit scope, ie. within the companion object of the type
class trait, or within the companion object of the type for which we're defining the instance.

For example, given the `Functor` type class and some type `Foo`, we would prefer to define the instance `Functor[Foo]`
in either the companion object of `Functor` or the companion object of `Foo`. In this case, whenever `Functor` and
`Foo` are visible, `Functor[Foo]` can be implicitly resolved without any further imports.

Conversely, an implicit definition which is defined _outside_ its implicit scope will only be resolved as an implicit
parameter if it is directly accessible, ie. if it is imported or defined in the current or an enclosing scope. This
would be the case if the instance `Functor[Foo]` were provided by a third-party, neither in the companion object of
`Functor` nor in the companion object of `Foo`. Type class instances defined in this way are _orphans_.

## Why are orphans a problem for automatically derived and subclass type class instances?

Pulling the above together we can conclude,

+ Orphan type class instances must be imported (because they're not in the implicit scope).
+ Orphan type class instances will trump non-orphan type class instances (because directly accessible implicits are
  found during the first phase of implicit resolution before the implicit scope is consulted).

For many purposes these are exacly the semantics we want: orphan type class instances are often used locally to
provide an alternative to the default instance of a type class which has been provided for a given type. In this
scenario we want the directly accessible local definition to trump the definitions in the implicit scope.

However, these aren't the semantics we want for automatically derived or subclass type class instances. Here we want
hand written instances, which will typically be defined in the implicit scope, to take precedence over derived
instances. This directly conflicts with the need to import orphans.

To complicate matters still further, it's common for there to be a default type class instance for a very general type
such as `AnyRef` or `Any`. This will typically be defined as a lowest priority instance in the companion object of the
type class trait. We will want derived and subclass instances to have a higher priority than these very general
instances.

In other words, we want derived and subclass instances to have a priority in-between hand-crafted specific instances
(in the implicit scope) and very general fallback instances (also in the implicit scope). This is very difficult to
achieve with orphan instances.

## Dealing with derived and subclass orphans

[shapeless][shapeless] has an evolving set of mechanisms which help to deal with this problem, but it would be even
better if we didn't have to deal with it at all, by not producing orphan instances in the first place. One way of
achieving this would be for projects which provide type classes to also publish derived and subclass instances
directly. However, in the derived case this forces a dependency on shapeless on those projects, which might be too
heavyweight for them, and it limits shapeless's ability to evolve independently; and in the subclass case it couples
subclasses and superclasses which can be problematic across module boundaries.

This project is a proof of concept of an alternative mechanism, an _export hook_, which allows derived and subclass
instances to be inserted into the implicit scope with the appropriate priority _without_ requiring either a shapeless
dependency or coupling between subclasses and superclasses, hence respecting module boundaries.

Instead the only dependency would be this project, which currently defines only the following two traits, two trival
value classes, one annotation and a couple of small Scala macros to support them,

```scala
trait Exporter0[S[_]]  {
  implicit def exports[F[t] >: S[t], T](implicit st: S[T]): Export0[F, T] =
    macro ExportMacro.exportsImpl0[F, T]
}

trait Exporter1[S[_[_]]]  {
  implicit def exports[F[t[_]] >: S[t], T[_]](implicit st: S[T]): Export1[F, T] =
    macro ExportMacro.exportsImpl1[F, T]
}

class Export0[F[_], T](val instance: F[T]) extends AnyVal

class Export1[F[_[_]], T[_]](val instance: F[T]) extends AnyVal

class exported[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ExportMacro.exportedImpl
}
```

(value classes and macro inlining have been used to eliminate all runtime overhead relative to directly importing
external instances).

Additionally, the project providing type classes, and other parties, would have to follow the conventions below,
...

## As seen by the type class provider

The participating type class provider has to include a hook for a type class deriver. That implies a dependency on
this project, and a hook trait as part of the stack of prioritized companion object traits,

```scala
import export._

trait Tc[T] {
  // Type class defns ...
}

object Tc extends TcLowPriority {
  // Instances which should be higher priority than derived
  // or subclass instances should be defined here ...
}

// Derived and subclass instances of Tc are automatically included here ...
@exported[Tc]
trait TcLowPriority {
  // Instances which should be lower priority than derived
  // or subclass instances should be defined here ...
}
```

## As seen by a type class deriver

A type class deriver has to provide an exporter instance which is able to provide instances of the requested types.
They can do that using shapeless or any other suitable mechanism,

```scala
import export._, tc._, shapeless._

// Automatically derive instances of Tc[T] for all T with a shapeless
// Generic instance

trait DerivedTc[T] extends Tc[T]

object DerivedTc extends Exporter0[DerivedTc] {
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

## As seen by a type class subclass

A type class subclass has to provide an exporter instance which is able to provide instances of the requested types.
Given that the subclass instances are automatically instances of their superclasses this is trivial,

```scala
import export._, tc._

trait TcSub[T] extends Tc[T]

object TcSub extends Exporter0[TcSub] {
  implicit val fooInst: TcSub[Foo] = ...
}

```

## As seen by the type class user

The type class user should import both the type class and the type class deriver.

```scala
import tc._
import TcDeriver.exports // for derived instances
import TcSub.exports     // for subclass instances
```

If the type class user doesn't want derived or subclass instances they simply omit the corresponding import in which
case they will only see underived base instances.

## Feedback wanted!

This is a proof of concept ... please create issues here or hop on the [shapeless][shapeless-gitter] or
[cats][cats-gitter] gitter channels and let us know what you think.

## Participation

The export-hook project supports the [Typelevel][typelevel] [code of conduct][codeofconduct] and wants all of its
channels (Gitter, github, etc.) to be welcoming environments for everyone.


## Building export-hook

export-hook is built with SBT 0.13.8 or later, and its master branch is built with Scala 2.11.7 by default.

## Contributors

+ Your name here :-)

[sls-7.2]: http://scala-lang.org/files/archive/spec/2.11/07-implicits.html#implicit-parameters
[sls-6.26.3]: http://scala-lang.org/files/archive/spec/2.11/06-expressions.html#overloading-resolution
[eed3si9n]: https://twitter.com/eed3si9n
[import-tax]: http://eed3si9n.com/revisiting-implicits-without-import-tax
[shapeless]: https://github.com/milessabin/shapeless
[shapeless-gitter]: https://gitter.im/milessabin/shapeless
[cats-gitter]: https://gitter.im/non/cats
[typelevel]: http://typelevel.org/
[codeofconduct]: http://typelevel.org/conduct.html
