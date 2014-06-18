package com.seeta.problems.lists

import com.seeta.problems.lists.FirstTenSolutions._
import org.scalatest.FunSuite

/**
 * This class is test class for `FirstTenSolutions`
 *
 * @author Seeta (Ramayya) Vadali
 */
class FirstTenSolutionsSuit extends FunSuite {
  val emptyList = List()
  val listOfInt = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

  test("last element list: empty") {
    assert(last(emptyList) === None)
  }

  test("last element list: 1, 2, 3") {
    assert(last(listOfInt) === Some(9))
  }

  test("last element list: banana and apple") {
    assert(last(List("banana", "apple")) === Some("apple"))
  }
  test("last but one element list: empty") {
    assert(last(emptyList) === None)
  }

  test("last but one element list: 1, 2, 3") {
    assert(lastButOne(listOfInt) === Some(8))
  }

  test("last but one element list: 1") {
    assert(lastButOne(List(1)) === None)
  }

  test("last but one element list: banana and apple") {
    assert(lastButOne(List("banana", "apple")) === Some("banana"))
  }

  test("nth element: list of int: 5th") {
    assert(nth(5, listOfInt) === Some(5))
  }

  test("nth element: list of int : 7th") {
    assert(nth(10, listOfInt) === None)
  }

  test("length: empty list") {
    assert(length(emptyList) === 0)
  }

  test("length: list of int") {
    assert(length(listOfInt) === 9)
  }

  test("reverse: empty list") {
    assert(reverse(emptyList) === emptyList)
  }

  test("reverse: list of int") {
    assert(reverse(listOfInt) === (9 to 1 by -1).toList)
  }

  test("palindrome: empty ") {
    assert(isPalindrome(emptyList))
  }

  test("palindrome: 1 2 3 2 1 ") {
    assert(isPalindrome(List(1, 2, 3, 2, 1)))
    assert(isPalindrome(List(1, 2, 2, 1)))
  }

  test("palindrome: 1 2 3 1") {
    assert(!isPalindrome(List(1, 2, 3, 1)))
  }

  test("flatten: ") {
    assert(flatten(List(List(1, 1), 2, List(3, List(List(5), 8)))) === List(1, 1, 2, 3, 5, 8))
  }
}
