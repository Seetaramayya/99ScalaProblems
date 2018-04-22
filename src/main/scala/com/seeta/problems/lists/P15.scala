package com.seeta.problems.lists

object P15 {
  /**
    * P 15. Duplicate the elements of a list n times.
    */
  def duplicateN[T](n: Int, input: List[T]): List[T] = {
    input.flatMap(List.fill[T](n)(_))
  }
}
