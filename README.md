# export-hook: minimal dependency support for expanding type class implicit scope

This project provides minimal infrastructure to support the inclusion of derived, subclass and other orphan instances
in the implicit scope of a type class without imposing heavyweight dependencies on the type class provider.

[![Build Status](https://api.travis-ci.org/milessabin/export-hook.png?branch=master)](https://travis-ci.org/milessabin/export-hook)
[![Stories in Ready](https://badge.waffle.io/milessabin/export-hook.png?label=Ready)](https://waffle.io/milessabin/export-hook)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/milessabin/export-hook)
[![Maven Central](https://img.shields.io/maven-central/v/org.typelevel/export-hook_2.11.svg)](https://maven-badges.herokuapp.com/maven-central/org.typelevel/export-hook_2.11)

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

## Dealing with derived, subclass and other orphans

[shapeless][shapeless] has an evolving set of mechanisms which help to deal with this problem, but it would be even
better if we didn't have to deal with it at all, and not produce orphan instances in the first place. One way of
achieving this would be for projects which provide type classes to also publish derived and subclass instances
directly. However, in the derived case this forces a dependency on shapeless onto those projects, which might be too
heavyweight for them, and it limits shapeless's ability to evolve independently; and in the subclass case it couples
subclasses and superclasses which can be problematic across module boundaries.

This project is a proof of concept of an alternative mechanism, an _export hook_, which allows derived, subclass and
other orphan instances to be inserted into the implicit scope of a type class with the appropriate priority _without_
requiring either a shapeless dependency or coupling between subclasses and superclasses, hence respecting module
boundaries.

Instead the only dependency is this project, which has a very small runtime footprint and no further dependencies.
Value classes and macro inlining have been used to eliminate all runtime overhead relative to directly importing
external instances.

The project providing type classes, and the other parties, follow the conventions below (`Encoder`, `Decoder`,
`Codec`, `DerivedEncoder` and `DerivedDecoder` are example user type classes) ...

## The type class provider

The participating type class provider includes a hook for exporters of instances such as type class derivers or
subclasses. That implies a dependency on this project, and a hook trait as part of the stack of prioritized companion
object traits,

```scala
import export._

trait Encoder[T] {
  // Type class defns ...
}

object Encoder extends EncoderLowPriority {
  // Instances which should be higher priority than derived
  // or subclass instances should be defined here ...
}

// Derived, subclass and other instances of Encoder are automatically included here ...
@imports[Encoder]
trait EncoderLowPriority {
  // Instances which should be lower priority than imported
  // instances should be defined here ...
}
```

(Below we assume a similar set of definitions for the `Decoder` type class).

## The provider of derived, subclass or other instances

A type class deriver provides instances for the requested types using shapeless or any other suitable mechanism and
indicates that they are to be exported by adding the `@exports` annotation to its companion object,

```scala
import export._, shapeless._

// Automatically derive instances of Encoder[T] for all T with a shapeless
// Generic instance

trait DerivedEncoder[T] extends Encoder[T]

@exports
object DerivedEncoder {
  implicit def hnil: DerivedEncoder[HNil] = ...

  implicit def hcons[H, T <: HList]
    (implicit hd: Encoder[H], tl: Lazy[DerivedEncoder[T]]): DerivedEncoder[H :: T] = ...

  implicit def cnil: DerivedEncoder[CNil] = ...

  implicit def ccons[H, T <: Coproduct]
    (implicit hd: Encoder[H], tl: Lazy[DerivedEncoder[T]]): DerivedEncoder[H :+: T] = ...

  implicit def gen[T, R]
    (implicit gen: Generic.Aux[T, R], mtcr: DerivedEncoder[R]): DerivedEncoder[T] = ...
}
```

(Below we assume a similar set of definitions for the `DerivedDecoder` type class).

A type class subclass provides instances which are automatically instances of their superclasses by virtue of the
subtype relationship,

```scala
import export._

trait Codec[T] extends Encoder[T] with Decoder[T]

@exports(Subclass)
object Codec {
  implicit val fooInst: Codec[Foo] = ...
}
```

Instances can be exported with different relative priorities. In the derivation example they are exported with the
default priority, which is appropiate for instances constructed using type class derivation. In the subclass example
we specify the `Subclass` priority explicitly as an argument to the `@export` annotation. The available instance
priorities are, in order from highest to lowest priority,

+ `HighPriority`

  A catch-all priority higher than any other defined here.
+ `Orphan`

  User provided explicit orphan instances.
+ `Subclass`

  Instances provided by subclasses, ie. a `Semigroup[T]` provided by a `Monoid[T]`.
+ `Algebraic`

  Instances provided by a combination of instances of other classes, combined according to their characteristic laws,
  ie. a `Monoid[T]` provided by a combination of a `Semigroup[T]` with a `Zero[T]`.
+ `Instantiated`

  Instances provided by instantiating a higher kinded instances at some first order type, ie. a `Monoid[List[T]]`
  provided by instantiating `MonoidK[List]` at `Int`.
+ `Generic` (default priority if not explicitly specified)

  Instances provided by type class derivation using shapeless or any other suitable mechanism.
+ `Default`

  Instances which are acceptable in the last resort.
+ `LowPriority`

  A catch-all priority lower than any other defined here.

## The type class user

The type class user should import both the type class and the type class deriver or subclass exports,

```scala
import Encoder
import DerivedEncoder.exports._  // for derived instances
import Codec.exports._           // for subclass instances
```

If the type class user doesn't want derived or subclass instances they simply omit the corresponding import, in which
case they will only see underived base instances.

## Locally modifying the instance priority ordering

The priority ordering of the instance categories is a reasonable default which should do what the user expects in
almost all circumstances. However, it's important to have an escape hatch for the rare cases where we need to do
something different. export-hook provides a mechanism for defining a local priority ordering via an implicit
definition of type `ExportPriority`. The local ordering will be in force only where that implicit definition is in
scope,

```scala
import export._

object CustomPrioritization {
  implicit val priority =
    ExportPriority[
      ExportHighPriority,
      ExportOrphan,
      ExportSubclass,
      ExportAlgebraic,
      ExportGeneric,      // give generic instances a higher priority than instances
      ExportInstantiated, // constructed from higher kinded instances
      ExportDefault,
      ExportLowPriority
    ]
}

object ScopeWithDefaultPriority {
  // Here a Monoid[List[Int]] constructed from a MonoidK[List] will be
  // selected using the default prioritization ...

  implicitly[Monoid[List[Int]]]
}

object ScopeWithCustomPriority {
  import CustomPrioritization._ // instance of ExportPriority now in scope ...

  // Here a Monoid[List[Int]] provided by type class derivation will be
  // selected using the our custom prioritization ...

  implicitly[Monoid[List[Int]]]
}
```

## Bundling and reexporting type class instances

It is sometimes convenient to make multiple orphan type class instances available via a single import. This is
supported via the `@reexport` annotation. In the example below all the instances defined by `DerivedEncoder` and
`DerivedDecoder` are made available in the current scope by a single import,

```scala
trait DerivedEncoder[T] extends Encoder[T]
@exports
object DerivedEncoder {
  // Encoder derivation here ...
}

trait DerivedDecoder[T] extends Decoder[T]
@exports
object DerivedDecorer {
  // Decoder derivation here ...
}

// Reexport instances of both type classes via a single object
@reexports[DerivedEncoder, DerivedDecoder]
object derivedcodecs

// Client code ...

import derivedcodecs._ // single import

// Instances of both DerivedEncoder and DerivedDecoder are now available via
// the implicit scope of Encoder and Decoder ...
```

Reexported instances will have the same priority that they were initially exported with.

## Exporting and reexporting individual instances

Sometimes it's a little heavyweight to create a new subclass simply to be able to export a handful of ad hoc
instances. In this case individual instance definitions can be exported by using a val/method level `@export`
definition marking the definition for export and providing a priority,

```scala
@exports
object InstantiatedEmptyK {
  @export(Instantiated)
  implicit def instantiate[F[_], T](implicit ekf: EmptyK[F]): Empty[F[T]] = ekf.synthesize[T]
}
```

Here we synthesize an instance of the Alleycats `Empty` type class from an instance of the higher-kinded `EmptyK` type
class. The resulting definition is exported with the `Instantiated` priority and made available by importing in the
usual way,

```scala
import InstantiatedEmptyK.exports._

Empty[List[Int]] // synthesized from EmptyK[List]
```

These individual instances can also be bundled and reexported,

```scala
@reexports(InstantiatedEmptyK)
object emptykinst
```

which allows imports of the form,

```scala
import emptykinst._

Empty[List[Int]] // synthesized from EmptyK[List]
```

## Current status

This is a young project and we are keen to get input from anyone who finds it useful ... please create issues here or
hop on the [gitter channel][exporthook-gitter].  Discussion is also welcome on the [shapeless][shapeless-gitter] and
[cats][cats-gitter] gitter channels ... please let us know what you think.

## Using export-hook

Binary release artefacts are published to the [Sonatype OSS Repository Hosting service][sonatype] and synced to Maven
Central. Snapshots of the master branch are built using [Travis CI][ci] and automatically published to the Sonatype
OSS Snapshot repository. To include the Sonatype repositories in your SBT build you should add,

```scala
resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)
```

Builds are available for Scala 2.11.x and 2.10.x for Scala JDK and Scala.js.  The main line of development for
export-hook 1.0.0 is Scala 2.11.7 supported via the macro paradise compiler plugin.

```scala
scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.typelevel" %% "export-hook" % "1.0.2",
  "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
)
```

## Binary compatibility

As of version 1.0.3 macro-compat uses [MiMa][mima] to verify binary compatibility within minor versions. export-hook
is binary compatible within minor versions from 1.0.2 onwards.

## Building export-hook

export-hook is built with SBT 0.13.9 or later, and its master branch is built with Scala 2.11.7 by default.

## Participation

The export-hook project supports the [Typelevel][typelevel] [code of conduct][codeofconduct] and wants all of its
channels (Gitter, github, etc.) to be welcoming environments for everyone.

## Projects using export-hook

+ [circe][circe]
+ [kittens][kittens]

## Contributors

+ Alistair Johnson <alistair.johnson@johnsonusm.com> [@AlistairUSM](https://twitter.com/AlistairUSM)
+ Miles Sabin <miles@milessabin.com> [@milessabin](https://twitter.com/milessabin)
+ Your name here :-)

[sls-7.2]: http://scala-lang.org/files/archive/spec/2.11/07-implicits.html#implicit-parameters
[sls-6.26.3]: http://scala-lang.org/files/archive/spec/2.11/06-expressions.html#overloading-resolution
[eed3si9n]: https://twitter.com/eed3si9n
[import-tax]: http://eed3si9n.com/revisiting-implicits-without-import-tax
[shapeless]: https://github.com/milessabin/shapeless
[exporthook-gitter]: https://gitter.im/milessabin/export-hook
[shapeless-gitter]: https://gitter.im/milessabin/shapeless
[cats-gitter]: https://gitter.im/non/cats
[typelevel]: http://typelevel.org/
[codeofconduct]: http://typelevel.org/conduct.html
[sonatype]: https://oss.sonatype.org/index.html#nexus-search;quick~export-hook
[ci]: https://travis-ci.org/milessabin/export-hook
[circe]: https://github.com/travisbrown/circe
[kittens]: https://github.com/milessabin/kittens
