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
    list.length
  }

  /**
   * P 05. Reverse a list.
   * @param list input
   * @tparam T type of element
   * @return reversed list
   */
  def reverse[T] (list: List[T]): List[T] = {
    // list.reverse
    (list foldLeft List[T]())((xs, x) => x :: xs)
  }

  /**
   * P 06. Finds out whether give list is a palindrome.
   * @param list input
   * @tparam T type of element
   * @return true if given list is palindrome otherwise false
   */
  def isPalindrome[T](list: List[T]): Boolean = {
    val reverse = list.reverse
    list == reverse
  }

  /**
   * P 07. Flatten a nested list structure.
   * @param list input
   * @return flattened list
   */
  def flatten(list: List[Any]):List[Any] = {
    def recursive(l: List[Any]):List[Any] = {
      val r = l.foldRight(List[Any]())((x, xs) => x match {
        case ys: List[Any] => ys ::: xs
        case y: Any => x :: xs
      })

      if (r.exists(x => x.isInstanceOf[List[Any]])) recursive(r) else r
    }
    recursive(list)
  }
}
