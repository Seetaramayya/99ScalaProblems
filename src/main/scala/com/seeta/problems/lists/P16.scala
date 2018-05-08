package com.seeta.problems.lists

object P16 {
  /**
    * P 16. Drop every Nth element from a list.
    */
  def drop[T](position: Int, input: List[T]): List[T] = {
    if (position <= 0 ) {
      input
    } else {
      input.zipWithIndex.filterNot {
        case (_, index) => (index + 1) % position == 0
      }.map(_._1)
    }
  }
}
