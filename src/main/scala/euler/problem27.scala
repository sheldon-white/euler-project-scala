package euler

import scala.collection.mutable._
import scala.math._
import BigInt._
import java.io._

// Considering quadratics of the form:
// n^2 + an + b, where |a| < 1000 and |b| < 1000
// where |n| is the modulus/absolute value of n
// e.g. |11| = 11 and |−4| = 4
// Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

object Problem27 {
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
//    println("Problem27: " + maxCycle)
  }
}