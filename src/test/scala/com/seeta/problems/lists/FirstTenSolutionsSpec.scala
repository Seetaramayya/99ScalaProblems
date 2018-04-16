package com.seeta.problems.lists

import com.seeta.problems.lists.FirstTenSolutions._
import org.scalatest.WordSpec
import org.scalatest._

class FirstTenSolutionsSpec extends WordSpec with Matchers {
  "Compress" should {
    "replace consecutive elements with the single element" in {
      compress(List()) shouldBe List()
      compress(List(1, 1)) shouldBe List(1)
      compress(List(1, 1, 2)) shouldBe List(1, 2)
      compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) shouldBe List('a, 'b, 'c, 'a, 'd, 'e)
      compress(List(1, 1, 2, 3, 4, 4, 4, 5)) shouldBe List(1, 2, 3, 4, 5)
    }
  }

  "Pack" should {
    "extract consecutive elements into sub list" in {
      pack(List()) shouldBe List(List())
      pack(List(1)) shouldBe List(List(1))
      pack(List(1, 1)) shouldBe List(List(1, 1))
      pack(List(1, 2, 3)) shouldBe List(List(1), List(2), List(3))

      val expected = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

      pack(input) shouldBe expected
    }
  }

  "Encode" should {
    "throw an IllegalArgumentException for empty list" in {
      the[IllegalArgumentException] thrownBy {
        encode(List())
      } should have message "input should be NEL (non empty list)"
    }

    "extract consecutive elements into (NumberOfElements, Element) tuple(s)" in {
      encode(List(1)) shouldBe List((1, 1))
      encode(List(1, 1)) shouldBe List((2, 1))
      encode(List(1, 2)) shouldBe List((1, 1), (1, 2))
      encode(List(1, 1, 2, 2)) shouldBe List((2, 1), (2, 2))

      val input = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
      val expected = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

      encode(input) shouldBe expected
    }
  }
}
