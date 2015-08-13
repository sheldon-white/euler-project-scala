package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._

//The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
// Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

object Problem48 {
  
  def powerLastTenDigits(num: Int, power: Int): BigInt = {
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
    accum % BigInt(10000000000L)
  }
  
  def main(args: Array[String]) = {
    val total:BigInt = (1 to 1000).map(p => powerLastTenDigits(p, p)).sum % BigInt(10000000000L)
    println("Problem 48: " + total)
  }
}
