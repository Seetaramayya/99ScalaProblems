package com.seeta.problems.lists

object P14 {
  /**
    * P 14. Duplicate the elements of a list.
    *
    */
  def duplicate[T](input: List[T]): List[T] = {
    input.map(x => (x, x)).foldRight(List[T]()){
      case ((e1, e2), acc) => e1 :: e2 :: acc
    }
  }
}
