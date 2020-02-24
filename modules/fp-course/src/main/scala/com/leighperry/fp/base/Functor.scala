package com.leighperry.fp.base

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] =
    F

  implicit val instanceOption: Functor[Option] =
    new Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
        ???
    }

  implicit def instanceEither[E]: Functor[Either[E, *]] =
    new Functor[Either[E, *]] {
      override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] =
        ???
    }

  implicit val instanceList: Functor[List] =
    new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        ???
    }

  implicit def instanceTuple2[X]: Functor[(X, *)] =
    new Functor[(X, *)] {
      override def map[A, B](fa: (X, A))(f: A => B): (X, B) =
        ???
    }

  implicit def instanceFunction1[X]: Functor[X => *] =
    new Functor[X => *] {
      override def map[A, B](fa: X => A)(f: A => B): X => B =
        ???
    }
}
