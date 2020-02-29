package com.leighperry.fp.base

import scala.annotation.tailrec

// custom list type, Xnil instead of Nil, ::: instead of ::
sealed trait Xlist[+A] {
  override def toString: String =
    Xlist.show(this)
}

case object Xnil extends Xlist[Nothing]
final case class :::[A](head: A, tail: Xlist[A]) extends Xlist[A]

////

object Xlist {
  def apply[A](as: A*): Xlist[A] =
    fromList(as.toList)

  def fromList[A](as: List[A]): Xlist[A] =
    as.foldRight[Xlist[A]](Xnil) {
      (h: A, t: Xlist[A]) =>
        h ::: t
    }

  def show[A](as: Xlist[A]): String = {
    @tailrec
    def loop(prefix: String, l: Xlist[A]): String =
      l match {
        case Xnil =>
          prefix + ")"

        case :::(head, Xnil) =>
          prefix + head + ")"

        case :::(head, tail) =>
          loop(prefix + head + ",", tail)
      }

    loop("Xlist(", as)
  }

  def product(xl: Xlist[Int]): Int =
    xl.foldLeft[Int](_ * _, 1)

  def sum(xl: Xlist[Int]): Int =
    xl.foldLeft[Int](_ + _, 0)

  def flatten[A](xl: Xlist[Xlist[A]]): Xlist[A] =
    xl.foldRight[Xlist[A]](_ ++ _, Xnil)

  // if any option in the list is None, result is None
  // otherwise result is Some(List(...values...))
  def sequenceOptional[A](xl: Xlist[Option[A]]): Option[Xlist[A]] =
    xl.foldRight[Option[Xlist[A]]](
      (optA, optList) =>
        //optList.flatMap(list => optA.map[Xlist[A]](_ ::: list))
        for {
          list <- optList
          o <- optA
        } yield o ::: list,
      Some(Xnil)
    )

  def replicate[A](n: Int, a: A): Xlist[A] =
    fromList {
      (1 to n)
        .map(_ => a)
        .toList
    }

  //// syntax

  implicit class XlistOps[A](val xl: Xlist[A]) extends AnyVal {
    def :::(a: A): Xlist[A] =
      new :::(a, xl)

    def foldLeft[B](f: (B, A) => B, init: B): B = {
      // local imperative
      def isEmpty(l: Xlist[A]): Boolean =
        l match {
          case Xnil => true
          case _ => false
        }
      def split(l: Xlist[A]): (A, Xlist[A]) =
        l match {
          case Xnil => sys.error("impossible")
          case head ::: tail => (head, tail)
        }

      var acc = init
      var these = xl
      while (!isEmpty(these)) {
        val (head, tail) = split(these)
        acc = f(acc, head)
        these = tail
      }
      acc
    }

    // Naive implementation is not tail recursive
    def foldRight[B](f: (A, B) => B, init: B): B =
      xl match {
        case Xnil =>
          init

        case head ::: tail =>
          f(head, tail.foldRight(f, init))
      }

    def headOr(a: A): A =
      foldRight[A]((aa, _) => aa, a)

    def length: Int =
      foldLeft[Int]((b, _) => b + 1, 0)

    def map[B](f: A => B): Xlist[B] =
      foldRight[Xlist[B]]((a, b) => f(a) ::: b, Xnil)

    def filter(predicate: A => Boolean): Xlist[A] =
      foldRight[Xlist[A]]((a, b) => if (predicate(a)) a ::: b else b, Xnil)

    def ++(other: Xlist[A]): Xlist[A] =
      foldRight[Xlist[A]]((a, b) => a ::: b, other)

    def flatMap[B](f: A => Xlist[B]): Xlist[B] =
      foldRight[Xlist[B]](
        (a, b) => f(a) ++ b,
        Xnil
      )

    def find(predicate: A => Boolean): Option[A] =
      foldLeft[Option[A]](
        (optA, a) =>
          optA match {
            case Some(_) => optA
            case None => Some(a).filter(predicate)
          },
        None
      )

    def reverse: Xlist[A] =
      foldLeft[Xlist[A]]((l, a) => a ::: l, Xnil)
  }

}

////

object ListTest {
  def main(args: Array[String]): Unit = {
    println(Xlist())
    println(Xlist(1))
    println(Xlist(1, 2))
    println(Xlist(1, 2, 3))

    val list = Xlist(1, 2, 3, 4, 5, 6)
    println(Xlist.sum(list))

    println(list.headOr(123))
    println(list.length)
    println(list.map("*" * _))
    println(list.filter(_ % 2 == 0))
    println(list ++ Xlist(7, 8, 9))
    println(list.flatMap(n => Xlist("[", "*" * n, "]")))
    println(list.find(_ % 2 == 1))
    println(list.reverse)

    println(Xlist.sequenceOptional(Option(1) ::: Option(2) ::: Xnil))
    println(Xlist.sequenceOptional[Int](Option(1) ::: Option.empty[Int] ::: Xnil))
  }
}
