# derivation-hook: minimal dependency type class instance derivation support

This project is a proof of concept of minimal infrastructure to support non-orphan type class derivation without
imposing heavyweight dependencies on type class providers.

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

[shapeless-gitter]: https://gitter.im/milessabin/shapeless
[cats-gitter]: https://gitter.im/non/cats
