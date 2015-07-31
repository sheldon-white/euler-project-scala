package euler

import scala.collection.mutable._
import scala.math._

// The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719,
// are themselves prime.
// There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
// How many circular primes are there below one million?
object Problem35 {
  def is(i: Long) =
    if (i == 2) true
    else if ((i & 1) == 0) false // efficient div by 2
    else prime(i)

  def primes: Stream[Long] = 2 #:: prime3

  // performance: avoid redundant divide by two, so this starts at 3
  private val prime3: Stream[Long] = {
    def next(i: Long): Stream[Long] =
      if (prime(i))
        i #:: next(i + 2)
      else
        next(i + 2) // tail
    3 #:: next(5)
  }

  // assumes not even, check evenness before calling - perf note: must pass partially applied >= method
  private def prime(i: Long) =
    prime3 takeWhile (math.sqrt(i).>= _) forall { i % _ != 0 }
  
  def transformString(s:String):List[String] = 
    for(i <- s.length until 0 by - 1 toList) yield s.drop(i)+ s.take(i)
  
  def isCircular(num: Int): Boolean = {
    val permutations = transformString(num.toString())
    for (permutation <- permutations) {
      if (!is(permutation.toInt)) {
        return false
      }
    }
    return true
  }
  
  def main(args: Array[String]) = {
    var total = (2 to 1000000).filter(n => isCircular(n)).size
    println("Problem 34: " + total)    
  }
}
