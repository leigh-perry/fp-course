package com.leighperry.fp.base

import com.leighperry.fp.base.Xlist.fromList
import com.leighperry.fp.testsupport.BaseSpec
import com.leighperry.fp.testsupport.TestSupport._
import zio.test.Assertion._
import zio.test._

object XlistSpec
  extends BaseSpec(
    suite("Xlist")(
      testM("product matches List's implementation") {
        check(Gen.listOf(Gen.anyInt)) {
          stdList =>
            assert(
              Xlist.product(fromList(stdList)),
              equalTo(stdList.product)
            )
        }
      },
      testM("sum matches List's implementation") {
        check(Gen.listOf(Gen.anyInt)) {
          stdList =>
            assert(
              Xlist.sum(fromList(stdList)),
              equalTo(stdList.sum)
            )
        }
      },
      testM("flatten matches List's implementation") {
        check(Gen.listOf(Gen.listOf(Gen.alphaNumericString))) {
          stdList: List[List[String]] =>
            assert(
              Xlist
                .flatten(fromList(stdList).map(fromList))
                .toList,
              equalTo(stdList.flatten)
            )
        }
      },
      testM("sequenceOptional") {
        check(Gen.listOf(Gen.alphaNumericString)) {
          stdList =>
            val threshold = 10
            val options: List[Option[String]] =
              stdList.map(s => if (s.length > threshold) Option.empty[String] else Option(s))
            val expected: Option[List[String]] =
              if (stdList.exists(_.length > threshold)) Option.empty[List[String]]
              else Option(stdList)

            assert(
              Xlist
                .sequenceOptional(fromList(options))
                .map(_.toList),
              equalTo(expected)
            )
        }
      },
      testM("headOr matches List's implementation") {
        check(Gen.listOf(Gen.alphaNumericString), Gen.anyString) {
          (stdList, alt) =>
            assert(
              fromList(stdList)
                .headOr(alt),
              equalTo(stdList.headOption.getOrElse(alt))
            )
        }
      },
      testM("length matches List's implementation") {
        check(Gen.listOf(Gen.alphaNumericString)) {
          stdList =>
            assert(
              fromList(stdList).length,
              equalTo(stdList.length)
            )
        }
      },
      testM("map matches List's implementation") {
        check(Gen.listOf(Gen.alphaNumericString)) {
          stdList =>
            assert(
              fromList(stdList)
                .map(_.toUpperCase)
                .toList,
              equalTo(stdList.map(_.toUpperCase))
            )
        }
      },
      testM("filter matches List's implementation") {
        check(Gen.listOf(Gen.alphaNumericString)) {
          stdList =>
            assert(
              fromList(stdList)
                .filter(_.length > 4)
                .toList,
              equalTo(stdList.filter(_.length > 4))
            )
        }
      },
      testM("++ matches List's implementation") {
        check(Gen.listOf(Gen.alphaNumericString), Gen.listOf(Gen.alphaNumericString)) {
          (stdList1, stdList2) =>
            assert(
              (fromList(stdList1) ++ fromList(stdList2)).toList,
              equalTo(stdList1 ++ stdList2)
            )
        }
      },
      testM("flatMap matches List's implementation") {
        check(Gen.listOf(Gen.alphaNumericString)) {
          stdList =>
            assert(
              fromList(stdList)
                .flatMap(s => Xlist(s, s, s))
                .toList,
              equalTo(stdList.flatMap(s => List(s, s, s)))
            )
        }
      },
      testM("find matches List's implementation") {
        check(Gen.listOf(Gen.alphaNumericString)) {
          stdList =>
            assert(
              fromList(stdList)
                .find(_.length < 2),
              equalTo(stdList.find(_.length < 2))
            )
        }
      },
      testM("reverse matches List's implementation") {
        check(Gen.listOf(Gen.alphaNumericString)) {
          stdList =>
            assert(
              fromList(stdList)
                .reverse
                .toList,
              equalTo(stdList.reverse)
            )
        }
      },
      testM("double reverse = original") {
        check(Gen.listOf(Gen.alphaNumericString)) {
          stdList =>
            assert(
              fromList(stdList)
                .reverse
                .reverse
                .toList,
              equalTo(stdList)
            )
        }
      }
    )
  )
