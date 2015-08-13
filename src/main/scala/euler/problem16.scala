package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._
import BigInt._
import util.Time._
// Problem 16
// 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
// What is the sum of the digits of the number 2^1000?

object Problem16 {
  
	def main(args: Array[String]) = {
    time {
      val two10: BigInt = 1024
      val two50: BigInt = two10 * two10 * two10 * two10 * two10
      val two250 = two50 * two50 * two50 * two50 * two50
      val two1000 = two250 * two250 * two250 * two250
		  println("Problem 15: " + two1000.toString.toList.foldLeft(0)(_ + _ - '0'))
    }
	}
}
