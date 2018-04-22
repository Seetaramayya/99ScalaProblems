package com.seeta.problems.lists

import com.seeta.problems.lists.P14.duplicate
import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks

class P14Spec extends WordSpec with PropertyChecks with Matchers {
  "Duplicate" should {
    "hold the property for every list" in {
      forAll { (input: List[Int]) =>
        val output = duplicate(input)
        val filteredOutput = output.grouped(2).filterNot {
          case x :: y :: Nil => x == y
          case _ => fail("undesired state, only 2 elements lists should produce")
        }
        assert(filteredOutput.isEmpty)
      }
    }
  }
}
