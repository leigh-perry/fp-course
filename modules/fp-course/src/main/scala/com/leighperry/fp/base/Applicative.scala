package com.leighperry.fp.base

trait Applicative[F[_]] extends Functor[F] {

  def pure[A](x: A): F[A]
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  //// Haskell-idiomatic <*>

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    ???

  //// lift functions

  def map2[A, B, Z](fa: F[A], fb: F[B])(f: (A, B) => Z): F[Z] =
    map(product(fa, fb)) {
      f.tupled
    }

  def map3[A0, A1, A2, Z](f0: F[A0], f1: F[A1], f2: F[A2])(f: (A0, A1, A2) => Z): F[Z] =
    map(product(f0, product(f1, f2))) {
      case (a0, (a1, a2)) => f(a0, a1, a2)
    }

  def map4[A0, A1, A2, A3, Z](f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3])(f: (A0, A1, A2, A3) => Z): F[Z] =
    map(product(f0, product(f1, product(f2, f3)))) {
      case (a0, (a1, (a2, a3))) => f(a0, a1, a2, a3)
    }

  def map5[A0, A1, A2, A3, A4, Z](f0: F[A0], f1: F[A1], f2: F[A2], f3: F[A3], f4: F[A4])(
    f: (A0, A1, A2, A3, A4) => Z
  ): F[Z] =
    map(product(f0, product(f1, product(f2, product(f3, f4))))) {
      case (a0, (a1, (a2, (a3, a4)))) => f(a0, a1, a2, a3, a4)
    }

  // etc
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] =
    F

  implicit val instanceOption: Applicative[Option] =
    new Applicative[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
        Functor.instanceOption.map(fa)(f)

      override def pure[A](x: A): Option[A] =
        ???

      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
        ???
    }

  implicit def instanceEither[E]: Applicative[Either[E, *]] =
    new Applicative[Either[E, *]] {
      override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] =
        Functor.instanceEither.map(fa)(f)

      override def pure[A](x: A): Either[E, A] =
        ???

      override def product[A, B](fa: Either[E, A], fb: Either[E, B]): Either[E, (A, B)] =
        ???
    }

  implicit val instanceList: Applicative[List] =
    new Applicative[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        Functor.instanceList.map(fa)(f)

      override def pure[A](x: A): List[A] =
        ???

      // cross-product
      override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
        ???
    }

  implicit def instanceFunction1[X]: Applicative[X => *] =
    new Applicative[X => *] {
      override def map[A, B](fa: X => A)(f: A => B): X => B =
        Functor.instanceFunction1.map(fa)(f)

      override def pure[A](x: A): X => A =
        ???

      override def product[A, B](fa: X => A, fb: X => B): X => (A, B) =
        ???
    }
}
