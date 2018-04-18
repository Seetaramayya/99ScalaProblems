package com.seeta.problems.lists

object P13 {
  /**
    * P 13. Run-length encoding of a list (direct solution).
    *
    * Implement the so-called run-length encoding data compression method directly.
    * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
    *
    */
  def encodeDirect[T](input: List[T]): List[(Int, T)] = {
    input.foldRight(List[(Int, T)]()) {
      case (elem, Nil)                       => (1, elem) :: Nil
      case (elem, (n, x) :: xs) if x == elem => (n + 1, x) :: xs
      case (elem, acc)                       => (1, elem) :: acc
    }
  }
}
