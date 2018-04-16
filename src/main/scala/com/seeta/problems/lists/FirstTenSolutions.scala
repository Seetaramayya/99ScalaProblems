package com.seeta.problems.lists

import scala.None

/**
 * This class have first 10 solutions of Werner Hett problems. Later Phil Gold altered
 * them for scala adoption.
 *
 * Problems can be find in [[http://aperiodic.net/phil/scala/s-99/ phil gold web site]]
 *
 * @author Seeta (Ramayya) Vadali
 */
object FirstTenSolutions {
  /**
   * P 01. Finds the last element of a list.
   * @param list in which last element will be searched
   * @tparam T element type
   * @return Some(T) if element finds otherwise None
   */
  def last[T](list: List[T]): Option[T] = {
    if (list.isEmpty) None
    else Some(list.last)
  }

  /**
   * P 02. Finds the last but one element of a list.
   * @param list in which lat but one element will be searche
   * @tparam T element type
   * @return if element find then returns Some(T) otherwise None
   */
  def lastButOne[T](list: List[T]): Option[T] = {
    list match {
      case Nil => None
      case x :: y :: Nil => Some(x)
      case x :: xs => lastButOne(xs)
    }
  }

  /**
   * P 03. Find the Kth element of a list.
   * @param position position of element
   * @param list in which nth element will be searched
   * @tparam T element type
   * @return if element find then returns Some(T) otherwise None
   */
  def nth[T](position: Int, list: List[T]):Option[T] = {
    val p = position -1
    def recursive(l: List[T], n: Int):Option[T] = {
      l match {
        case Nil => None
        case x :: xs if n == 0 => Some(x)
        case x :: xs if n > 0 => recursive(xs, n - 1)
        case _ => None
      }
    }
    if (p < list.length && p >= 0) recursive(list, p) else None
  }

  /**
   * P 04. Finds the number of elements of a list.
   * @param list length will be find of this list
   * @tparam T type of element
   * @return size of list
   */
  def length[T] (list: List[T]): Int = {
    list match {
      case Nil => 0
      case x :: xs => 1 + length(xs)
    }
  }

  /**
   * P 05. Reverse a list.
   * @param list input
   * @tparam T type of element
   * @return reversed list
   */
  def reverse[T] (list: List[T]): List[T] = {
    // list.reverse (too easy)
    // (list foldLeft List[T]())((xs, x) => x :: xs) is okay
    list match {
      case Nil => Nil
      case x :: xs => reverse(xs) :+ x //still using List method
    }
  }

  /**
   * P 06. Finds out whether give list is a palindrome.
   * @param list input
   * @tparam T type of element
   * @return true if given list is palindrome otherwise false
   */
  def isPalindrome[T](list: List[T]): Boolean = {
    val reversed = reverse(list)
    list == reversed
  }

  /**
   * P 07. Flatten a nested list structure.
   * @param list input
   * @return flattened list
   */
  def flatten(list: List[Any]):List[Any] = {
    def recursive(l: List[Any]):List[Any] = {
      l match {
        case (x :List[Any]) :: xs => recursive(x) ::: recursive(xs)
        case x :: xs => x :: recursive(xs)
        case Nil => Nil
      }
    }
    recursive(list)
  }

  /**
   * P 08. Eliminate consecutive duplicates of list elements.
   */
  def compress[T](list: List[T]): List[T] = {
    list.foldRight(List[T]()) {
      case (elem, List()) => elem :: Nil
      case (elem, acc) if acc.head == elem => acc
      case (elem, acc) => elem :: acc
    }
  }

  /**
    * P 09. Eliminate consecutive duplicates of list elements.
    *
    * If a list contains repeated elements they should be placed in separate sublists.
    */
  def pack[T](input: List[T]): List[List[T]] = {
    input.foldRight(List(List[T]())) {
      case (elem, List(List()))                 => List(List(elem))
      case (elem, acc) if acc.head.head == elem => (elem :: acc.head) :: acc.tail
      case (elem, acc)                          => List(elem) :: acc
    }
  }

  /**
    * P 10. Run-length encoding of a list.
    *
    * Use the result of problem P09 to implement the so-called run-length encoding data compression method.
    * Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates
    * of the element E.
    */
  def encode[T](input: List[T]): List[(Int, T)] = {
    if (input.isEmpty) throw new IllegalArgumentException("input should be NEL (non empty list)")
    else {
      pack(input).map(innerList => (innerList.size, innerList.head))
    }
  }
}
