package com.seeta.problems.lists

import com.seeta.problems.lists.P11.encodeModified
import org.scalatest.{Matchers, WordSpec}

class P11Spec extends WordSpec with Matchers {
  "Encode Modified" should {
    "extract consecutive elements into (NrElem, Elem) tuple(s) when only duplicate exits" in {
      encodeModified(List(1)) shouldBe List(1)
      encodeModified(List(1, 1)) shouldBe List((2, 1))
      encodeModified(List(1, 2)) shouldBe List(1, 2)
      encodeModified(List(1, 1, 2, 2)) shouldBe List((2, 1), (2, 2))

      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expected = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

      encodeModified(input) shouldBe expected
    }
  }
}
