package com.seeta.problems.lists

import com.seeta.problems.lists.P16.drop
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class DropPropertySpecification extends WordSpec with PropertyChecks with Matchers {
  val position: Gen[Int] = Gen.choose(1, 100)
  val generatedIntList: Gen[List[Int]] = Gen.listOfN(Random.nextInt(1000), Gen.choose(-1000, 1000))
  "Drop" should {

    "hold the property: all dropped elements divisible by position" in {
      forAll(position, generatedIntList) { (position: Int, generatedIntList: List[Int]) =>
        val output = drop(position, generatedIntList)
        if (position == 1) {
          output shouldBe List()
        } else {
          val missingElements = generatedIntList.filter(output.contains(_))
          missingElements.forall(_ % position == 0)
        }
      }
    }

    "hold the property: input size == (result + dropped elements) size" in {
      forAll(position, generatedIntList) { (position: Int, generatedIntList: List[Int]) =>
        val result = drop(position, generatedIntList)
        if (position >= 1) {
          val inputSize = generatedIntList.size
          val zeroIndexPosition = position - 1
          val positions: List[Int] = (zeroIndexPosition until inputSize by position).toList
          result.size + positions.size shouldBe inputSize
        } else {
          result shouldBe generatedIntList
        }
      }
    }
  }
}
