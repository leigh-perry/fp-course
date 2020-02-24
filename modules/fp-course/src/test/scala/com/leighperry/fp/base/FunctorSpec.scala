package com.leighperry.fp.base

import com.leighperry.fp.base.FSupport._
import com.leighperry.fp.base.FunctorTests._
import com.leighperry.fp.testsupport.BaseSpec
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object FunctorSpec
  extends BaseSpec(
    suite("Functor")(
      fIdentity[Option]("Option (Some)", some),
      fIdentity[Option]("Option (None)", none),
      composition[Option]("Option (Some)", some),
      composition[Option]("Option (None)", none),
      ////
      fIdentity[Either[String, *]]("Either (Right)", right),
      fIdentity[Either[String, *]]("Either (Left)", left),
      composition[Either[String, *]]("Either (Right)", right),
      composition[Either[String, *]]("Either (Left)", left),
      ////
      fIdentity[List]("List", list),
      composition[List]("List", list),
      ////
      fIdentity[(String, *)]("Tuple2", second),
      composition[(String, *)]("Tuple2", second),
      ////
      fIdentity[String => *]("Function1", function, functionEqualTo),
      composition[String => *]("Function1", function, functionEqualTo)
    )
  )

////

object FunctorTests {
  def fIdentity[F[_]: Functor](
    name: String,
    pure: Int => F[Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name identity") {
      check(Gen.anyInt) {
        i =>
          assert(
            Functor[F].map(pure(i))(identity),
            assertion(pure(i))
          )
      }
    }

  def composition[F[_]: Functor](
    name: String,
    pure: Int => F[Int],
    assertion: F[Int] => Assertion[F[Int]] = equalTo(_)
  ): ZSpec[Random, Nothing, String, Unit] =
    testM(s"$name composition") {
      check(
        Gen.anyInt,
        Gen.function[Random, Int, Int](Gen.anyInt),
        Gen.function[Random, Int, Int](Gen.anyInt)
      ) {
        (i, f, g) =>
          val F = Functor[F]

          val r1: F[Int] = F.map(pure(i))(f)
          val r2: F[Int] = F.map(r1)(g)
          assert(
            F.map(pure(i))(g compose f),
            assertion(r2)
          )
      }
    }
}
