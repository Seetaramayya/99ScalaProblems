package com.seeta.problems.lists

import com.seeta.problems.lists.P14.duplicate
import org.scalatest.{Matchers, WordSpec}

class P14Spec extends WordSpec with Matchers {
  "Duplicate" should {
    "duplicate each element" in {
      duplicate(List()) shouldBe List()
      duplicate(List(1)) shouldBe List(1, 1)
      duplicate(List(1, 2)) shouldBe List(1, 1, 2, 2)
      duplicate(List("Seeta")) shouldBe List("Seeta", "Seeta")

      val input = List('a, 'b, 'b, 'c, 'd)
      val expected = List('a, 'a, 'b, 'b, 'b, 'b, 'c, 'c, 'd, 'd)
      duplicate(input) shouldBe expected
    }
  }
}
