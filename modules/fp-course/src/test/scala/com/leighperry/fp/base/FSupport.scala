package com.leighperry.fp.base

import zio.test.Assertion.Render.param
import zio.test._

object FSupport {

  def some[A]: A => Option[A] = Some(_)
  def none[A]: A => Option[A] = _ => None
  def right[A]: A => Either[String, A] = Right(_)
  def left[A]: A => Either[String, A] = _ => Left("something")
  def list[A]: A => List[A] = List(_)
  def second[A]: A => (String, A) = ("ignored", _)
  def function[A]: A => String => A = a => _ => a

  val functionEqualTo: (String => Int) => Assertion[String => Int] =
    (expectedFn: String => Int) =>
      Assertion.assertion("functionEquivalence")(param(expectedFn)) {
        actual =>
          actual("samplestring") == expectedFn("samplestring") // TODO property test
      }

}
