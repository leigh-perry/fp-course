package com.leighperry.fp.base

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](ffa: F[F[A]]): F[A] =
    ???
}

object Monad {
  def apply[F[_]](implicit F: Monad[F]): Monad[F] =
    F

  implicit val instanceOption: Monad[Option] =
    new Monad[Option] {
      override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
        ???

      override def pure[A](a: A): Option[A] =
        Applicative.instanceOption.pure(a)

      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
        Applicative.instanceOption.product(fa, fb)

      override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
        Applicative.instanceOption.map(fa)(f)
    }

  implicit def instanceEither[E]: Monad[Either[E, *]] =
    new Monad[Either[E, *]] {
      override def flatMap[A, B](fa: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        ???

      override def pure[A](a: A): Either[E, A] =
        Applicative.instanceEither.pure(a)

      override def product[A, B](fa: Either[E, A], fb: Either[E, B]): Either[E, (A, B)] =
        Applicative.instanceEither.product(fa, fb)

      override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] =
        Applicative.instanceEither.map(fa)(f)
    }

  implicit val instanceList: Monad[List] =
    new Monad[List] {
      override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        ???

      override def pure[A](a: A): List[A] =
        Applicative.instanceList.pure(a)

      override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
        Applicative.instanceList.product(fa, fb)

      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        Applicative.instanceList.map(fa)(f)
    }

  implicit def instanceFunction1[X]: Monad[X => *] =
    new Monad[X => *] {
      // Dependency injection :-)
      override def flatMap[A, B](fa: X => A)(f: A => X => B): X => B =
        ???

      override def pure[A](a: A): X => A =
        Applicative.instanceFunction1.pure(a)

      override def product[A, B](fa: X => A, fb: X => B): X => (A, B) =
        Applicative.instanceFunction1.product(fa, fb)

      override def map[A, B](fa: X => A)(f: A => B): X => B =
        Applicative.instanceFunction1.map(fa)(f)
    }
}
