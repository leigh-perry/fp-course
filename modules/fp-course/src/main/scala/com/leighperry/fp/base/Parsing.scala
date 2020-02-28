package com.leighperry.fp.base

import com.leighperry.fp.base.ApplicativeSyntax._
import com.leighperry.fp.base.FunctorSyntax._
import com.leighperry.fp.base.MonadSyntax._

object Parsing {

  // A parser for things
  // Is a function from strings
  // To lists of pairs Of things and strings

  type Chars = Xlist[Char]
  type Input = Chars

  sealed trait ParseResult[+A]

  object ParseResult {
    final case object UnexpectedEof extends ParseResult[Nothing]
    final case class ExpectedEof(input: Input) extends ParseResult[Nothing]
    final case class UnexpectedChar(c: Char) extends ParseResult[Nothing]
    final case class UnexpectedString(s: Chars) extends ParseResult[Nothing]
    final case class Result[A](remainder: Input, result: A) extends ParseResult[A]

    implicit def instanceFunctor: Functor[ParseResult] =
      new Functor[ParseResult] {
        override def map[A, B](fa: ParseResult[A])(f: A => B): ParseResult[B] =
          fa match {
            case UnexpectedEof => UnexpectedEof
            case ExpectedEof(input) => ExpectedEof(input)
            case UnexpectedChar(c) => UnexpectedChar(c)
            case UnexpectedString(s) => UnexpectedString(s)
            case Result(input, result) => Result(input, f(result))
          }
      }

    def isErrorResult[A](r: ParseResult[A]): Boolean =
      r match {
        case Result(_, _) => false
        case _ => true
      }
  }

  import ParseResult._

  def onResult[A, B](r: ParseResult[A], onResult: (Input, A) => ParseResult[B]): ParseResult[B] =
    r match {
      case UnexpectedEof => UnexpectedEof
      case ExpectedEof(input) => ExpectedEof(input)
      case UnexpectedChar(c) => UnexpectedChar(c)
      case UnexpectedString(s) => UnexpectedString(s)
      case Result(remainder, a) => onResult(remainder, a)
    }

  ////

  final case class Parser[T](f: Input => ParseResult[T])

  object Parser {

    implicit val instanceFunctor: Functor[Parser] =
      new Functor[Parser] {
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          Parser[B] {
            input =>
              val prx: ParseResult[A] = fa.f(input)
              prx.map(f)
          }
      }

    implicit val instanceApplicative: Applicative[Parser] =
      new Applicative[Parser] {
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          instanceFunctor.map(fa)(f)

        override def pure[A](x: A): Parser[A] =
          Parser[A] {
            input =>
              Result(input, x)
          }

        override def product[A, B](fa: Parser[A], fb: Parser[B]): Parser[(A, B)] =
          Parser[(A, B)] {
            input =>
              val pra: ParseResult[A] = fa.f(input)
              pra match {
                case UnexpectedEof =>
                  UnexpectedEof
                case ExpectedEof(input) =>
                  ExpectedEof(input)
                case UnexpectedChar(c) =>
                  UnexpectedChar(c)
                case UnexpectedString(s) =>
                  UnexpectedString(s)
                case Result(remainder, a) =>
                  fb.f(remainder).map {
                    b: B =>
                      (a, b)
                  }
              }
          }
      }

    implicit val instanceMonad: Monad[Parser] =
      new Monad[Parser] {
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          instanceApplicative.map(fa)(f)

        override def pure[A](x: A): Parser[A] =
          instanceApplicative.pure(x)

        override def product[A, B](fa: Parser[A], fb: Parser[B]): Parser[(A, B)] =
          instanceApplicative.product(fa, fb)

        override def flatMap[A, B](fa: Parser[A])(f: A => Parser[B]): Parser[B] =
          Parser[B] {
            input =>
              fa.f(input) match {
                case UnexpectedEof =>
                  UnexpectedEof
                case ExpectedEof(input) =>
                  ExpectedEof(input)
                case UnexpectedChar(c) =>
                  UnexpectedChar(c)
                case UnexpectedString(s) =>
                  UnexpectedString(s)
                case Result(remainder, a) =>
                  val pb: Parser[B] = f(a)
                  pb.f(remainder)
              }
          }
      }

  }

  ////

  def parse[A](p: Parser[A], input: Input): ParseResult[A] =
    p.f(input)

  // Produces a parser that always fails with @UnexpectedChar@ using the given character
  def unexpectedCharParser[A](c: Char): Parser[A] =
    Parser {
      _ =>
        UnexpectedChar(c)
    }

  def slist(s: String): Xlist[Char] =
    Xlist.fromList(s.toList)

  // Return a parser that always returns the given parse result
  //----
  // def main(args: Array[String]): Unit = assert(isErrorResult(parse(constantParser(UnexpectedEof), slist("abc"))))
  //----
  def constantParser[A](r: ParseResult[A]): Parser[A] =
    Parser(Function.const(r))

  // Return a parser that succeeds with a character off the input or fails with an error if the input is empty
  //----
  // def main(args: Array[String]): Unit = assert(Result(Xlist('b', 'c'), 'a') == parse(character, slist("abc")))
  //----
  // def main(args: Array[String]): Unit = assert(isErrorResult(parse(character, Xnil)))
  //----
  val character: Parser[Char] =
    Parser {
      case Xnil =>
        UnexpectedEof
      case head ::: tail =>
        Result(tail, head)
    }

  // Return a parser that always succeeds with the given value and consumes no input
  //----
  // def main(args: Array[String]): Unit = assert(Result(Xlist('a', 'b', 'c'), 3) == parse((valueParser(3)), slist("abc")))
  //----
  def valueParser[A](a: A): Parser[A] =
    Parser[A] {
      input =>
        Result(input, a)
    }

  implicit class ParserOps1[A](private val p1: Parser[A]) extends AnyVal {
    // Return a parser that tries the first parser for a successful value
    //   * If the first parser succeeds then use this parser
    //   * If the first parser fails, try the second parser
    def |||(p2: Parser[A]): Parser[A] =
      Parser[A] {
        input =>
          p1.f(input) match {
            case r @ Result(_, _) =>
              r
            case _ =>
              p2.f(input)
          }
      }
  }
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist(), 'v') == parse(character ||| valueParser('v'), Xnil))
  //   assert(Result(Xlist(), 'v') == parse(constantParser(UnexpectedEof) ||| valueParser('v'), Xnil))
  //   assert(Result(Xlist('b', 'c'), 'a') == parse(character ||| valueParser('v'), slist("abc")))
  //   assert(Result(Xlist('a', 'b', 'c'), 'v') == parse(constantParser(UnexpectedEof) ||| valueParser('v'), slist("abc")))
  // }
  //----

  // Monad tests
  //----
  // def main(args: Array[String]): Unit = {
  //   val p: Parser[Char] =
  //     character.flatMap {
  //       c =>
  //         if (c == 'x') character
  //         else valueParser('v')
  //     }
  //   assert(Result(Xlist('b', 'c'), 'v') == parse(p, slist("abc")))
  //   assert(Result(Xlist(), 'v') == parse(p, slist("a")))
  //   assert(Result(Xlist('b', 'c'), 'a') == parse(p, slist("xabc")))
  //   assert(isErrorResult(parse(p, Xnil)))
  //   assert(isErrorResult(parse(p, slist("x"))))
  // }
  //----

  //  Return a parser that produces a character but fails if
  //    * The input is empty
  //    * The character does not satisfy the given predicate
  //
  //  /Tip:/ The @flatMap@, @unexpectedCharParser@ and @character@ functions will be helpful here
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist('b', 'c'), 'A') == parse(satisfy(_.isUpper), slist("Abc")))
  //   assert(isErrorResult(parse(satisfy(_.isUpper), slist("abc"))))
  // }
  //----
  def satisfy(predicate: Char => Boolean): Parser[Char] =
    character.flatMap {
      c =>
        if (predicate(c)) c.pure[Parser]
        else unexpectedCharParser(c)
    }

  // Return a parser that produces the given character but fails if
  //   * The input is empty
  //   * The produced character is not equal to the given character
  //
  // /Tip:/ Use the @satisfy@ function.
  def is(c: Char): Parser[Char] =
    satisfy(_ == c)

  // Return a parser that produces a character between '0' and '9' but fails if
  //   * The input is empty
  //   * The produced character is not a digit
  //
  // /Tip:/ Use the @satisfy@ and @Data.Char#isDigit@ functions
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist(),'9') == parse (digit ,slist ("9")))
  //   assert(Result(Xlist('2','3'),'1') == parse (digit ,slist ("123")))
  //   assert(isErrorResult (parse (digit, Xnil)))
  //   assert(isErrorResult (parse (digit, slist("hello"))))
  // }
  //----
  val digit: Parser[Char] =
    satisfy(_.isDigit)

  //
  // Return a parser that produces a whitespace character but fails if
  //   * The input is empty
  //   * The produced character is not white space
  //
  // /Tip:/ Use the @satisfy@ function
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist(), ' ') == parse(space, slist(" ")))
  //   assert(Result(Xlist(' ', 'z'), '\n') == parse(space, slist("\n z")))
  //   assert(isErrorResult(parse(space, Xnil)))
  //   assert(isErrorResult(parse(space, slist("a"))))
  // }
  //----
  val space: Parser[Char] =
    satisfy(_.isWhitespace)

  implicit class ParserOps2[A](private val p1: Parser[A]) extends AnyVal {
    // Return a parser that prepends the result of the first parser onto the result of
    // the second. Pronounced "cons parser"
    //
    // /Tip:/ Use @Applicative.map2@
    //
    // >>> parse (digit .:. valueParser "hello") "321"
    // Result >21< "3hello"
    def <:>(p2: Parser[Xlist[A]]): Parser[Xlist[A]] =
      p1.map2(p2)(_ ::: _)
  }
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist('b', 'c'), Xlist('a')) == parse(character <:> valueParser(Xnil), slist("abc")))
  //   assert(
  //     Result(Xlist('2', '1'), Xlist('3', 'h', 'e', 'l', 'l', 'o')) == parse(
  //       character <:> valueParser(slist("hello")),
  //       slist("321")
  //     )
  //   )
  // }
  //----

  // Return a parser that continues producing a list of values from the given parser.
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xnil, Xnil) == parse(list(character), Xnil))
  //   assert(Result(Xlist('a', 'b', 'c'), Xlist('1', '2', '3')) == parse(list(digit), slist("123abc")))
  //   assert(Result(Xlist('a', 'b', 'c'), Xnil) == parse(list(digit), slist("abc")))
  //   assert(Result(Xnil, Xlist('a', 'b', 'c')) == parse(list(character), slist("abc")))
  //   assert(Result(Xnil, Xlist('v', 'v', 'v')) == parse(list(character *> valueParser('v')), slist("abc")))
  //   assert(Result(Xnil, Xnil) == parse(list(character *> valueParser('v')), Xnil))
  // }
  //----
  // 0 or many Ps ===  0 P || 1 or many Ps
  def list[A](p: Parser[A]): Parser[Xlist[A]] =
    // list1(p) ||| (Xnil: Xlist[A]).pure[Parser] // stack overflow in scala
    p.flatMap {
      a =>
        a.pure[Parser] <:> list(p)
    } ||| (Xnil: Xlist[A]).pure[Parser]

  // Return a parser that produces at least one value from the given parser then
  // continues producing a list of values from the given parser (to ultimately produce a non-empty list).
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xnil, Xlist('a', 'b', 'c')) == parse(list1(character), slist("abc")))
  //   assert(Result(Xnil, Xlist('v', 'v', 'v')) == parse(list1(character *> valueParser('v')), slist("abc")))
  //   assert(isErrorResult(parse(list1(character *> valueParser('v')), Xnil)))
  // }
  //----
  // 1 or many Ps === 1 P and then 0 or many Ps
  def list1[A](p: Parser[A]): Parser[Xlist[A]] =
    p <:> list(p)

}
