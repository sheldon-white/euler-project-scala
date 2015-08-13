package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._

// The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: 
// (i) each of the three terms are prime
// (ii) each of the 4-digit numbers are permutations of one another.
// There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property,
// but there is one other 4-digit increasing sequence.
// What 12-digit number do you form by concatenating the three terms in this sequence?
object Problem49 {
  
  def primePermutations(num: Int): List[Int] = {
    num.toString.toList.permutations.map(_.mkString("").toInt).filter(_ > 999).filter(Prime.isPrime(_)).toList.sorted
  }
  
  def containsSequence(primes: List[Int]) = {
    for (p1 <- 0 until primes.length;
         p2 <- p1+1 until primes.length;
         p3 <- p2+1 until primes.length;
         if primes(p2) - primes(p1) == primes(p3) - primes(p2))
        yield(List(primes(p1), primes(p2), primes(p3)))
  }
   
  def main(args: Array[String]) = {
    val solutions = HashSet[String]()
    for (num <- 1000 to 9999) {
       val p = primePermutations(num)
       val m = containsSequence(p)
      // println(m)
       solutions += m.flatten.mkString("")
    }
    solutions.remove("")
    solutions.remove("148748178147")
    println("Problem 49: " + solutions)
  }
}
