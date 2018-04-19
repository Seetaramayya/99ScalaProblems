package com.seeta.problems.lists

import com.seeta.problems.lists.P13.encodeDirect
import org.scalatest.{Matchers, WordSpec}

class P13Spec extends WordSpec with Matchers {
  "EncodeDirect" should {
    "encode run-length encoding data compression method" in {
      encodeDirect(List()) shouldBe List()
      encodeDirect(List('a)) shouldBe List((1, 'a))
      encodeDirect(List('a, 'a)) shouldBe List((2, 'a))
      encodeDirect(List('a, 'b)) shouldBe List((1, 'a), (1, 'b))

      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expected = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
      encodeDirect(input) shouldBe expected
    }
  }
}
