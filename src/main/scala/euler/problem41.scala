package euler

import scala.collection.mutable._
import scala.math._
import util.Prime

// We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
// For example, 2143 is a 4-digit pandigital and is also prime.
// What is the largest n-digit pandigital prime that exists?

object Problem41 {   
  def permutationsOfLen(len: Int): List[Int] = {
    (for (i <- 1 to len) yield i).permutations.toList.map(_.mkString("").toInt).sorted.reverse
  }

  def permutations: List[Int] = {
    (for (len <- 9 to 1 by -1) yield permutationsOfLen(len)).flatten.toList
  }

  
  def main(args: Array[String]) = {
    println("Problem 41: " + permutations.find(Prime.isPrime(_)).get)
  }
}
