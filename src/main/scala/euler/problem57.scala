package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// It is possible to show that the square root of two can be expressed as an infinite continued fraction.
// √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
// By expanding this for the first four iterations, we get:
// 1 + 1/2 = 3/2 = 1.5
// 1 + 1/(2 + 1/2) = 7/5 = 1.4
// 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
// 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
// The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985,
// is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
// In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

object Problem57 {
  
  def next(t: Tuple2[BigInt, BigInt]) = (t._1 + 2 * t._2, t._1 + t._2)
  
  def main(args: Array[String]) = {
    time {
      var matches = 0
      var t = (BigInt(1), BigInt(1))
      for (i <- 1 to 1000) {
        t = next(t)
        if (t._1.toString.length > t._2.toString.length) {
          matches += 1
        }
      }
      println("Problem 57: " + matches)
    }
  }
}
