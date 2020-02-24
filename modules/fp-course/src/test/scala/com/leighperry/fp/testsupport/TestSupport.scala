package com.leighperry.fp.testsupport

import com.leighperry.fp.base.Xlist
import zio.duration._
import zio.test.environment.TestEnvironment
import zio.test.{ DefaultRunnableSpec, TestAspect, ZSpec }

abstract class BaseSpec(spec: => ZSpec[TestEnvironment, Any, String, Any])
  extends DefaultRunnableSpec(spec, List(TestAspect.timeout(1.minute)))

object TestSupport {
  implicit class MoreXlistOps[A](l: Xlist[A]) {
    def toList: List[A] =
      l.foldRight[List[A]]((h: A, t: List[A]) => h :: t, Nil)

  }
}
