package com.seeta.problems.lists

import scala.annotation.tailrec

object P12 {
  /**
    * P 12. Decode a run-length encoded list.
    *
    * Given a run-length code list generated as specified in problem P10,
    * construct its uncompressed version.
    *
    */
  def decode[T](input: List[(Int, T)]): List[T] = {
    @tailrec
    def addTimes(elem: T, times: Int, acc: List[T]): List[T] = if (times <= 0) acc else {
      addTimes(elem, times - 1, elem :: acc)
    }

    input.foldRight(List[T]()) {
      case ((times, elem), acc) => addTimes(elem, times, acc)
    }
  }
}
