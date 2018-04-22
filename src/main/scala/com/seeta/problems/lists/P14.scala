package com.seeta.problems.lists

object P14 {
  /**
    * P 14. Duplicate the elements of a list.
    */
  def duplicate[T](input: List[T]): List[T] = {
    input.flatMap(x => List(x, x))
  }
}
