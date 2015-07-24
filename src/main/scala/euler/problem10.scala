package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

// Problem 10: Sum of all primes below 2 million?
object Problem10 {
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
  
  def main(args: Array[String]) = {
    var smallPrimeSum = (for (i <- 2 to 2000000; if is(i)) yield(i)).foldLeft(0) (_ + _)
    println("Problem 10: " + smallPrimeSum)
  }
}
