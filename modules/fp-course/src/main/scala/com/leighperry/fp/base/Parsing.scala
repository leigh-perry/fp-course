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
      ???

    def isErrorResult[A](r: ParseResult[A]): Boolean =
      ???
  }

  import ParseResult._

  def onResult[A, B](r: ParseResult[A], onResult: (Input, A) => ParseResult[B]): ParseResult[B] =
    ???

  ////

  final case class Parser[T](f: Input => ParseResult[T])

  object Parser {

    implicit val instanceFunctor: Functor[Parser] =
      ???

    implicit val instanceApplicative: Applicative[Parser] =
      new Applicative[Parser] {
        override def map[A, B](fa: Parser[A])(f: A => B): Parser[B] =
          instanceFunctor.map(fa)(f)

        override def pure[A](x: A): Parser[A] =
          ???

        override def product[A, B](fa: Parser[A], fb: Parser[B]): Parser[(A, B)] =
          ???
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
          ???
      }

  }

  ////

  def parse[A](p: Parser[A], input: Input): ParseResult[A] =
    ???

  // Produces a parser that always fails with @UnexpectedChar@ using the given character
  def unexpectedCharParser[A](c: Char): Parser[A] =
    ???

  // a helper for writing tests
  def slist(s: String): Xlist[Char] =
    Xlist.fromList(s.toList)

  // Return a parser that always returns the given parse result
  //----
  // def main(args: Array[String]): Unit = assert(isErrorResult(parse(constantParser(UnexpectedEof), slist("abc"))))
  //----
  def constantParser[A](r: ParseResult[A]): Parser[A] =
    ???

  // Return a parser that succeeds with a character off the input or fails with an error if the input is empty
  //----
  // def main(args: Array[String]): Unit = assert(Result(Xlist('b', 'c'), 'a') == parse(character, slist("abc")))
  //----
  // def main(args: Array[String]): Unit = assert(isErrorResult(parse(character, Xnil)))
  //----
  val character: Parser[Char] =
    ???

  // Return a parser that always succeeds with the given value and consumes no input
  //----
  // def main(args: Array[String]): Unit = assert(Result(Xlist('a', 'b', 'c'), 3) == parse((valueParser(3)), slist("abc")))
  //----
  def valueParser[A](a: A): Parser[A] =
    ???

  implicit class ParserOps1[A](private val p1: Parser[A]) extends AnyVal {
    // Return a parser that tries the first parser for a successful value
    //   * If the first parser succeeds then use this parser
    //   * If the first parser fails, try the second parser
    def |||(p2: Parser[A]): Parser[A] =
      ???
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
    ???

  // Return a parser that produces the given character but fails if
  //   * The input is empty
  //   * The produced character is not equal to the given character
  // /Tip:/ Use the @satisfy@ function.
  def is(c: Char): Parser[Char] =
    ???

  // Return a parser that produces a character between '0' and '9' but fails if
  //   * The input is empty
  //   * The produced character is not a digit
  // /Tip:/ Use the @satisfy@ and @Char#isDigit@ functions
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist(),'9') == parse (digit ,slist ("9")))
  //   assert(Result(Xlist('2','3'),'1') == parse (digit ,slist ("123")))
  //   assert(isErrorResult (parse (digit, Xnil)))
  //   assert(isErrorResult (parse (digit, slist("hello"))))
  // }
  //----
  val digit: Parser[Char] =
    ???

  //
  // Return a parser that produces a whitespace character but fails if
  //   * The input is empty
  //   * The produced character is not white space
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
    ???

  implicit class ParserOps2[A](private val p1: Parser[A]) extends AnyVal {
    // Return a parser that prepends the result of the first parser onto the result of
    // the second. Pronounced "cons parser"
    def <:>(p2: Parser[Xlist[A]]): Parser[Xlist[A]] =
      ???
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

  // Return a parser that continues producing a list of values from the given parser
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
    ???

  // Return a parser that produces at least one value from the given parser then
  // continues producing a list of values from the given parser (to ultimately produce a non-empty list)
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xnil, Xlist('a', 'b', 'c')) == parse(list1(character), slist("abc")))
  //   assert(Result(Xnil, Xlist('v', 'v', 'v')) == parse(list1(character *> valueParser('v')), slist("abc")))
  //   assert(isErrorResult(parse(list1(character *> valueParser('v')), Xnil)))
  // }
  //----
  // 1 or many Ps === 1 P and then 0 or many Ps
  def list1[A](p: Parser[A]): Parser[Xlist[A]] =
    ???

  // Return a parser that produces one or more space characters
  // (consuming until the first non-space) but fails if
  //
  //   * The input is empty
  //
  //   * The first produced character is not a space
  // /Tip:/ Use the @list1@ and @space@ functions.
  val spaces1: Parser[Chars] =
    ???

  // Return a parser that produces a lower-case character but fails if
  //   * The input is empty
  //   * The produced character is not lower-case
  // /Tip:/ Use the @satisfy@ and @Char#isLower@ functions.
  val lower: Parser[Char] =
    ???

  // Return a parser that produces an upper-case character but fails if
  //   * The input is empty
  //   * The produced character is not upper-case
  // /Tip:/ Use the @satisfy@ and @Char#isUpper@ functions.
  val upper: Parser[Char] =
    ???

  // Return a parser that produces an alpha character but fails if
  //
  //   * The input is empty
  //
  //   * The produced character is not alpha
  // /Tip:/ Use the @satisfy@ and @Char#isLetterOrDigit@ functions.
  val alpha: Parser[Char] =
    ???

  // Return a parser that sequences the given list of parsers by producing all their results
  // but fails on the first failing parser of the list
  // /Tip:/ Optionally use @Xlist#foldRight@. If not, an explicit recursive call
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(
  //     Result(Xlist('d', 'e', 'f'), Xlist('a', 'x', 'C')) ==
  //       parse(sequenceParser(Xlist(character, is('x'), upper)), slist("axCdef"))
  //   )
  //   assert(isErrorResult(parse(sequenceParser(Xlist(character, is('x'), upper)), slist("abCdef"))))
  // }
  //----
  def sequenceParser[A](l: Xlist[Parser[A]]): Parser[Xlist[A]] =
    ???

  // Return a parser that produces the given number of values off the given parser
  // This parser fails if the given parser fails in the attempt to produce the given number of values
  // /Tip:/ Use @sequenceParser@ and @xlist.replicate@
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist('e', 'f'), Xlist('A', 'B', 'C', 'D')) == parse(thisMany(4, upper), slist("ABCDef")))
  //   assert(isErrorResult(parse(thisMany(4, upper), slist("ABcDef"))))
  // }
  //----
  def thisMany[A](n: Int, p: Parser[A]): Parser[Xlist[A]] =
    ???

  // Age: positive integer
  //
  // def main(args: Array[String]): Unit = {
  //   assert(UnexpectedEof == parse(ageParser, slist("")))
  //   assert(Result(Xlist('x'), 1) == parse(ageParser, slist("1x")))
  //   assert(Result(Xlist('x'), 120) == parse(ageParser, slist("120x")))
  //   assert(UnexpectedChar('x') == parse(ageParser, slist("x120")))
  // }
  val ageParser: Parser[Int] =
    ???

  // Write a parser for Person.firstName
  // /First Name: non-empty string that starts with a capital letter and is followed
  // by zero or more lower-case letters/
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist(), Xlist('A', 'b', 'c')) == parse(firstNameParser, slist("Abc")))
  //   assert(isErrorResult(parse(firstNameParser, slist("abc"))))
  // }
  //----
  val firstNameParser: Parser[Chars] =
    ???

  // Write a parser for Person.surname
  //
  // /Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters./
  // /Tip:/ Use @flatMap@, @pure@, @upper@, @thisMany@, @lower@ and @list@
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist(), Xlist('A', 'b', 'c', 'd', 'e', 'f')) == parse(surnameParser, slist("Abcdef")))
  //   assert(
  //     Result(Xlist(), Xlist('A', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l')) ==
  //       parse(surnameParser, slist("Abcdefghijkl"))
  //   )
  //   assert(isErrorResult((parse(surnameParser, slist("Abc")))))
  //   assert(isErrorResult((parse(surnameParser, slist("abc")))))
  // }
  //----
  def nOrMore[A](n: Int, p: Parser[A]): Parser[Xlist[A]] =
    ???

  val surnameParser: Parser[Chars] =
    ???

  // Write a parser for Person.smoker
  // /Smoker: character that must be @'y'@ or @'n'@/
  // /Tip:/ Use @is@ and @(|||)@./
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist('a', 'b', 'c'), true) == parse(smokerParser, slist("yabc")))
  //   assert(Result(Xlist('a', 'b', 'c'), false) == parse(smokerParser, slist("nabc")))
  //   assert(isErrorResult(parse(smokerParser, slist("abc"))))
  // }
  //----
  val smokerParser: Parser[Boolean] =
    ???

  // Write part of a parser for Person#phoneBody
  // This parser will only produce a string of digits, dots or hyphens
  // It will ignore the overall requirement of a phone number to
  // start with a digit and end with a hash (#)
  //
  // /Phone: string of digits, dots or hyphens .../
  // /Tip:/ Use @list@, @digit@, @(|||)@ and @is@
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist(), Xlist('1', '2', '3', '-', '4', '5', '6')) == parse(phoneBodyParser, slist("123-456")))
  //   assert(Result(Xlist('a', '5', '6'), Xlist('1', '2', '3', '-', '4')) == parse(phoneBodyParser, slist("123-4a56")))
  //   assert(Result(Xlist('a', '1', '2', '3', '-', '4', '5', '6'), Xlist()) == parse(phoneBodyParser, slist("a123-456")))
  // }
  // >>> parse phoneBodyParser "a123-456"
  // Result >a123-456< ""
  //----
  val phoneBodyParser: Parser[Chars] =
    ???

  // Write a parser for Person.phone
  // /Phone: ... but must start with a digit and end with a hash (#)./
  // /Tip:/ Use @flatMap@, @pure@, @digit@, @phoneBodyParser@ and @is@
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(Result(Xlist(), Xlist('1', '2', '3', '-', '4', '5', '6')) == parse(phoneParser, slist("123-456#")))
  //   assert(
  //     Result(Xlist('a', 'b', 'c'), Xlist('1', '2', '3', '-', '4', '5', '6')) ==
  //       parse(phoneParser, slist("123-456#abc"))
  //   )
  //   assert(isErrorResult(parse(phoneParser, slist("a123-456"))))
  // }
  //----
  val phoneParser: Parser[Chars] =
    ???

  // Write a parser for Person
  // /Tip:/ Use @Applicative.map5@
  //----
  // def main(args: Array[String]): Unit = {
  //   assert(isErrorResult(parse(personParser, slist(""))))
  //   assert(isErrorResult(parse(personParser, slist("12x Fred Clarkson y 123-456.789#"))))
  //   assert(isErrorResult(parse(personParser, slist("123 fred Clarkson y 123-456.789#"))))
  //   assert(isErrorResult(parse(personParser, slist("123 Fred Cla y 123-456.789#"))))
  //   assert(isErrorResult(parse(personParser, slist("123 Fred clarkson y 123-456.789#"))))
  //   assert(isErrorResult(parse(personParser, slist("123 Fred Clarkson x 123-456.789#"))))
  //   assert(isErrorResult(parse(personParser, slist("123 Fred Clarkson y 1x3-456.789#"))))
  //   assert(isErrorResult(parse(personParser, slist("123 Fred Clarkson y -123-456.789#"))))
  //   assert(isErrorResult(parse(personParser, slist("123 Fred Clarkson y 123-456.789"))))
  //   assert(Result(Xlist(), Person(123, Xlist('F', 'r', 'e', 'd'), Xlist('C', 'l', 'a', 'r', 'k', 's', 'o', 'n'), true, Xlist('1', '2', '3', '-', '4', '5', '6', '.', '7', '8', '9'))) == parse(personParser, slist("123 Fred Clarkson y 123-456.789#")))
  //   assert(Result(Xlist(), Person(123, Xlist('F', 'r', 'e', 'd'), Xlist('C', 'l', 'a', 'r', 'k', 's', 'o', 'n'), true, Xlist('1', '2', '3', '-', '4', '5', '6', '.', '7', '8', '9'))) == parse(personParser, slist("123    Fred    Clarkson    y    123-456.789#")))
  //   assert(Result(Xlist(' ', 'r', 'e', 's', 't'), Person(123, Xlist('F', 'r', 'e', 'd'), Xlist('C', 'l', 'a', 'r', 'k', 's', 'o', 'n'), true, Xlist('1', '2', '3', '-', '4', '5', '6', '.', '7', '8', '9'))) == parse(personParser, slist("123 Fred Clarkson y 123-456.789# rest")))
  // }
  //----
  final case class Person(age: Int, firstName: Chars, surname: Chars, smoker: Boolean, phone: Chars)

  val personParser: Parser[Person] =
    ???
}
