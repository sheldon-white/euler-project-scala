package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

//The square root of 2 can be written as an infinite continued fraction.
// sqrt(2) = 1 + 1/(2 + 1/(2 + 1/(2 +...
// The infinite continued fraction can be written, √2 = [1;(2)], (2) indicates that 2 repeats ad infinitum.
// What is most surprising is that the important mathematical constant,
// e = [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...].
//
// The first ten terms in the sequence of convergents for e are:
// 2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536, ...
// The sum of digits in the numerator of the 10th convergent is 1+4+5+7=17.
// Find the sum of digits in the numerator of the 100th convergent of the continued fraction for e.

object Problem65 {
  val seq = (1L to 400L).map(nextSegment(_)).flatten.toVector

  def calculatePartial(count: Int) = {
    val segment = seq.take(count - 1)
    var b = BigInt(1)
    var rev = segment.reverse
    var a = BigInt(rev(0))
    rev = rev.drop(1)
    for (n <- rev) {
      val aNext = n * a + b
      val bNext = a
      a = aNext
      b = bNext
    }
    (2 * a + b, a)
  }
  
  def nextSegment(n: Long) = {
    Vector(1, 2*n, 1)
  }
  
  def main(args: Array[String]) = {
    time {
      breakable {
        val r = calculatePartial(100)
        val sum = r._1.toString.toList.map(_ - '0').sum
       println("Problem 65: " + sum)
      }
    }
  }
}