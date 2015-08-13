package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// The prime 41, can be written as the sum of six consecutive primes:
// 41 = 2 + 3 + 5 + 7 + 11 + 13
// This is the longest sum of consecutive primes that adds to a prime below one-hundred.
// The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
// Which prime, below one-million, can be written as the sum of the most consecutive primes?
object Problem50 {
  //val maxPrime = 1000
  val maxPrime = 1000000
  val minSequenceLength = 400
  
  def calculateMaxStartOfSeq(smallPrimes: Vector[Long]): Int = {
    for (i <- 0 until smallPrimes.size) {
      if (smallPrimes(i) > maxPrime / minSequenceLength) {
        println(smallPrimes(i))
        return i
      }
    }
    return 0
  }

  def calculateSums(smallPrimes: Vector[Long]): Vector[Long] = {
    var primeSums = ListBuffer[Long]()
    var total: Long = 0;
    for (p <- 0 until smallPrimes.size) {
      primeSums += total
      total += smallPrimes(p)
    }
    primeSums toVector
  }

  def maxPrimeSequence(smallPrimes: Vector[Long]): Tuple2[Long, Int] = {
    val primeSums = calculateSums(smallPrimes)
    val maxStartIdx = calculateMaxStartOfSeq(smallPrimes)
    val maximii = new HashMap[Long, Int]()
    for (i <- 0 until maxStartIdx) {
      breakable {
        for (j <- i + 1 until smallPrimes.size; if j < primeSums.size - 1) {
          val sum = primeSums(j + 1) - primeSums(i)
          if (sum > maxPrime) {
            break
          }
          val seqLen = j - i + 1
          if (seqLen > minSequenceLength && Prime.isPrime(sum)) {
            println(i, j, ":", sum, seqLen)
            if (!maximii.contains(sum)) {
              maximii(sum) = seqLen
            } else if (maximii(sum) < sum) {
              maximii(sum) = seqLen
            }
          }
        }
      }
    }
    maximii.maxBy(_._2)
  }
  
  def main(args: Array[String]) = {
    time {
      val smallPrimes = Prime.primes.takeWhile(_ < maxPrime) toVector
      val maxSequence = maxPrimeSequence(smallPrimes)
      println("Problem 50: " + maxSequence)
    }
  }
}
