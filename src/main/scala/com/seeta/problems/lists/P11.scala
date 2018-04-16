package com.seeta.problems.lists

import com.seeta.problems.lists.FirstTenSolutions.pack

object P11 {
  /**
    * P 11. Modified run-length encoding.
    *
    * Modify the result of problem P10 in such a way that if an element has no
    * duplicates it is simply copied into the result list. Only elements with
    * duplicates are transferred as (N, E) terms.
    *
    */
  def encodeModified[T](input: List[T]): List[Any] = {
    if (input.isEmpty) throw new IllegalArgumentException("input should be NEL (non empty list)")
    else {
      pack(input).map {
        case head :: Nil => head
        case head :: xs  => (xs.length + 1, head)
      }
    }
  }
}
