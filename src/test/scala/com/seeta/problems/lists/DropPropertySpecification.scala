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
    "hold the property: output + dropped elements == input" in {
      forAll(position, generatedIntList) { (position: Int, generatedIntList: List[Int]) =>
        val output = drop(position, generatedIntList)
        if (position == 1) {
          output shouldBe List()
        } else {
          val groupedElements: List[List[Int]] = output.grouped(position - 1).toList
          val missingElementsPos: List[Option[Int]] = (1 to generatedIntList.length).filter(_ % position == 0).map(Some(_)).toList
          val missingElements: List[Option[Int]] = missingElementsPos.map {
            case Some(idx) => Some(generatedIntList(idx))
          }
          println(missingElements)
          val combinedElements: List[(List[Int], Option[Int])] = groupedElements.zipAll(missingElements, List(), None)
          combinedElements.flatMap {
            case (list, Some(element)) => list :+ element
            case (list, None)          => list
          } shouldBe generatedIntList
        }
      }
    }
  }
}
