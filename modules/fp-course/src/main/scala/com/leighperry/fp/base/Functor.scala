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
        fa match {
          case Some(a) => Some(f(a))
          case None => None
        }
    }

  implicit def instanceEither[E]: Functor[Either[E, *]] =
    new Functor[Either[E, *]] {
      override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] =
        fa match {
          case Left(e) => Left(e)
          case Right(a) => Right(f(a))
        }
    }

  implicit val instanceList: Functor[List] =
    new Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)
    }

  implicit def instanceTuple2[X]: Functor[(X, *)] =
    new Functor[(X, *)] {
      override def map[A, B](fa: (X, A))(f: A => B): (X, B) =
        (fa._1, f(fa._2))
    }

  implicit def instanceFunction1[X]: Functor[X => *] =
    new Functor[X => *] {
      override def map[A, B](fa: X => A)(f: A => B): X => B =
        f compose fa
    }
}

////

object FunctorSyntax {
  implicit class FunctorOpsF[F[_]: Functor, A](fa: F[A]) {
    def map[B](f: A => B): F[B] =
      Functor[F].map(fa)(f)
  }
}
