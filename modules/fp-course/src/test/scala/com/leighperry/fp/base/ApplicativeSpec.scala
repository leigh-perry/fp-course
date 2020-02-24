package com.leighperry.fp.base

import com.leighperry.fp.base.ApplicativeTests._
import com.leighperry.fp.base.FSupport._
import com.leighperry.fp.testsupport.BaseSpec
import zio.random.Random
import zio.test.Assertion.equalTo
import zio.test._

object ApplicativeSpec
  extends BaseSpec(
    suite("Applicative")(
      apIdentity[Option]("Option (Some)", some, some),
      apIdentity[Option]("Option (None)", none, none),
      homomorphism[Option]("Option (Some)", some, some),
      homomorphism[Option]("Option (None)", none, none),
      interchange[Option]("Option (Some)", some, some, some),
      interchange[Option]("Option (None)", none, none, none),
      composition[Option]("Option (Some)", some, some, some),
      composition[Option]("Option (None)", none, none, none),
      ////
      apIdentity[Either[String, *]]("Either (Right)", right, right),
      apIdentity[Either[String, *]]("Either (Left)", left, left),
      homomorphism[Either[String, *]]("Either (Right)", right, right),
      homomorphism[Either[String, *]]("Either (Left)", left, left),
      interchange[Either[String, *]]("Either (Right)", right, right, right),
      interchange[Either[String, *]]("Either (Left)", left, left, left),
      composition[Either[String, *]]("Either (Right)", right, right, right),
      composition[Either[String, *]]("Either (Left)", left, left, left),
      ////
      apIdentity[List]("List", list, list),
      homomorphism[List]("List", list, list),
      interchange[List]("List", list, list, list),
      composition[List]("List", list, list, list),
      ////
      apIdentity[String => *]("Function1", function, function, functionEqualTo),
      homomorphism[String => *]("Function1", function, function, functionEqualTo),
      interchange[String => *]("Function1", function, function, function, functionEqualTo),
      composition[String => *]("Function1", function, function, function, functionEqualTo)
    )
  )

object ApplicativeTests {
  def apIdentity[F[_]: Applicative](
    name: String,
    pure: Int => F[Int],
    pureF: (Int => Int) => F[Int => Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name identity") {
      check(Gen.anyInt) {
        i =>
          // pure id <*> v = v
          assert(
            Applicative[F].ap(pureF(identity))(pure(i)),
            assertion(pure(i))
          )
      }
    }

  def homomorphism[F[_]: Applicative](
    name: String,
    pure: Int => F[Int],
    pureF: (Int => Int) => F[Int => Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name homomorphism") {
      check(Gen.anyInt, Gen.function[Random, Int, Int](Gen.anyInt)) {
        (x, f) =>
          // pure f <*> pure x = pure (f x)
          assert(
            Applicative[F].ap(pureF(f))(pure(x)),
            assertion(pure(f(x)))
          )
      }
    }

  def interchange[F[_]: Applicative](
    name: String,
    pure: Int => F[Int],
    pureF: (Int => Int) => F[Int => Int],
    pureFF: ((Int => Int) => Int) => F[(Int => Int) => Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name interchange") {
      check(Gen.anyInt, Gen.function[Random, Int, Int](Gen.anyInt)) {
        (y: Int, uu: Int => Int) =>
          // u <*> pure y = pure ($ y) <*> u
          //   ($ y) is the function that supplies y as an argument to another function
          // ie
          // u <*> pure y = pure (\f -> f y) <*> u

          // ap(u, pure(y)) must always equal ap(pure(f => f(y)), u). This is known as the “interchange law”.
          // Here u is an applicative which holds some function A => B, and y is something of type A.
          val u: F[Int => Int] = pureF(uu)

          assert(
            Applicative[F].ap(u)(pure(y)),
            assertion(
              Applicative[F].ap(pureFF((f: Int => Int) => f(y)))(u)
            )
          )
      }
    }

  def composition[F[_]: Applicative](
    name: String,
    pure: Int => F[Int],
    pureF: (Int => Int) => F[Int => Int],
    pureFFF: ((Int => Int) => (Int => Int) => Int => Int) => F[(Int => Int) => (Int => Int) => Int => Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name composition") {
      check(Gen.anyInt, Gen.function[Random, Int, Int](Gen.anyInt), Gen.function[Random, Int, Int](Gen.anyInt)) {
        (y: Int, uu: Int => Int, vv: Int => Int) =>
          // pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
          // pure (\x y -> x . y) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition

          // u : F[B => C]
          // v : F[A => B]
          // w : F[A]
          // ap(ap(ap(pure(compose), u), v), w) must be equal to ap(u, ap(v, w))

          val u: F[Int => Int] = pureF(uu)
          val v: F[Int => Int] = pureF(vv)
          val w: F[Int] = pure(y)

          val F = Applicative[F]
          val compose: F[(Int => Int) => (Int => Int) => Int => Int] =
            pureFFF((f: Int => Int) => (g: Int => Int) => f compose g)

          // def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
          val a1: F[(Int => Int) => Int => Int] = F.ap(compose)(u)
          val a2 = F.ap(a1)(v)
          val a3 = F.ap(a2)(w)
          assert(
            a3,
            assertion(
              F.ap(u)(F.ap(v)(w))
            )
          )
      }
    }

}
