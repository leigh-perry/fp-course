package com.leighperry.fp.base

import com.leighperry.fp.base.FSupport._
import com.leighperry.fp.base.MonadTests._
import com.leighperry.fp.testsupport.BaseSpec
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object MonadSpec
  extends BaseSpec(
    suite("Monad")(
      flatMap[Option]("Option (Some)", some),
      flatMap[Option]("Option (None)", none),
      leftIdentity[Option]("Option (Some)", some),
      leftIdentity[Option]("Option (None)", none),
      rightIdentity[Option]("Option (Some)", some),
      rightIdentity[Option]("Option (None)", none),
      associativity[Option]("Option (Some)", some),
      associativity[Option]("Option (None)", none),
      ////
      flatMap[Either[String, *]]("Either (Right)", right),
      flatMap[Either[String, *]]("Either (Left)", left),
      leftIdentity[Either[String, *]]("Either (Right)", right),
      leftIdentity[Either[String, *]]("Either (Left)", left),
      rightIdentity[Either[String, *]]("Either (Right)", right),
      rightIdentity[Either[String, *]]("Either (Left)", left),
      associativity[Either[String, *]]("Either (Right)", right),
      associativity[Either[String, *]]("Either (Left)", left),
      ////
      flatMap[List]("List", list),
      leftIdentity[List]("List", list),
      rightIdentity[List]("List", list),
      associativity[List]("List", list),
      ////
      flatMap[String => *]("Function1", function, functionEqualTo),
      leftIdentity[String => *]("Function1", function, functionEqualTo),
      rightIdentity[String => *]("Function1", function, functionEqualTo),
      associativity[String => *]("Function1", function, functionEqualTo)
    )
  )

object MonadTests {
  def flatMap[F[_]: Monad](
    name: String,
    pure: Int => F[Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name fmap") {
      check(Gen.anyInt) {
        i =>
          assert(Monad[F].flatMap(pure(i))(i => pure(i + 1)), assertion(pure(i + 1)))
      }
    }

  def leftIdentity[F[_]: Monad](
    name: String,
    pure: Int => F[Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name left identity") {
      check(Gen.anyInt, Gen.function[Random, Int, Int](Gen.anyInt)) {
        (i, f) =>
          val fLifted = (j: Int) => pure(f(j))

          assert(
            Monad[F].flatMap(pure(i))(fLifted),
            assertion(
              pure(f(i))
            )
          )
      }
    }

  def rightIdentity[F[_]: Monad](
    name: String,
    pure: Int => F[Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name right identity") {
      check(Gen.anyInt, Gen.function[Random, Int, Int](Gen.anyInt)) {
        (i, f) =>
          val fLifted = (j: Int) => pure(f(j))

          assert(
            Monad[F].flatMap(pure(i))(fLifted),
            assertion(
              pure(f(i))
            )
          )
      }
    }

  def associativity[F[_]: Monad](
    name: String,
    pure: Int => F[Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name associativity") {
      check(
        Gen.anyInt,
        Gen.function[Random, Int, Int](Gen.anyInt),
        Gen.function[Random, Int, Int](Gen.anyInt)
      ) {
        (i, f, g) =>
          val fLifted: Int => F[Int] = (j: Int) => pure(f(j))
          val gLifted: Int => F[Int] = (j: Int) => pure(g(j))

          val gf = g compose f
          val gfLifted: Int => F[Int] = (j: Int) => pure(gf(j))

          val r1: F[Int] = Monad[F].flatMap(pure(i))(fLifted)
          val r2: F[Int] = Monad[F].flatMap(r1)(gLifted)

          assert(
            r2,
            assertion(
              Monad[F].flatMap(pure(i))(gfLifted)
            )
          )
      }
    }
}
