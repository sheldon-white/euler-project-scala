package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
// How many n-digit positive integers exist which are also an nth power?
object Problem63 {
    def power(num: Int, power: Int): BigInt = {
    val powerBits = power.toBinaryString.toList.reverse
    var accum: BigInt = 1
    var multiplier: BigInt = num
    //println(powerBits)
    for (i <- 0 until powerBits.length) {
      if (powerBits(i) == '1') {
        accum = accum * multiplier
      }
      multiplier = multiplier * multiplier
    }
    accum
  }

  def main(args: Array[String]) = {
    time {
      breakable {
        var ctr = 0
        for (p <- 1 to 200; n <- 1 to 9) {
          val num = power(n, p)
          if (num.toString.length == p) {
            println(n, p, num)
            ctr += 1
          }
        }
        println("Problem 63: " + ctr)
      }
    }
  }
}