package com.seeta.problems.lists

import com.seeta.problems.lists.P12.decode
import org.scalatest.{Matchers, WordSpec}

class P12Spec extends WordSpec with Matchers {
  "Decode" should {
    "reverse of encode of P10" in {
      decode(List()) shouldBe List()
      decode(List((3, 1))) shouldBe List(1, 1, 1)
      decode(List((1, 1), (1, 2), (1, 3))) shouldBe List(1, 2, 3)

      val input = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
      decode(input) shouldBe List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
    }
  }
}
