package com.seeta.problems.lists

import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

class P16Spec extends WordSpec with PropertyChecks with Matchers {
  "P16.drop" should {
    "succeed with empty list" in {
      P16.drop(2, List()) shouldBe List()
    }

    "drop all elements" in {
      P16.drop(1, List(1)) shouldBe List()
      P16.drop(1, List(1, 2, 3, 4)) shouldBe List()
    }

    "succeed with alternative element" in {
      P16.drop(2, List(1, 2)) shouldBe List(1)
      P16.drop(2, List(1, 2, 3)) shouldBe List(1, 3)
    }

    "succeed with 3rd element drop" in {
      P16.drop(3, List(1, 2)) shouldBe List(1, 2)
      P16.drop(3, List(1, 2, 3)) shouldBe List(1, 2)
      P16.drop(3, List(1, 2, 3, 4)) shouldBe List(1, 2, 4)
    }
  }
}
