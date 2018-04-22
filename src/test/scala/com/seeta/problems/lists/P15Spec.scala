package com.seeta.problems.lists

import com.seeta.problems.lists.P15.duplicateN
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class P15Spec extends WordSpec with PropertyChecks with Matchers {
  val times: Gen[Int] = Gen.choose(0, 100)
  val generatedIntList: Gen[List[Int]] = Gen.listOfN(Random.nextInt(1000), Gen.choose(-1000, 1000))

  "DuplicateN" should {
    "hold the property for every list with given number of times(n)" in {
      forAll(times, generatedIntList) { (times: Int, input: List[Int]) =>
        val output = duplicateN(times, input)
        val filteredOutput = output.grouped(times).filterNot(_.distinct.length == 1)
        filteredOutput.isEmpty shouldBe true
      }
    }
  }
}
