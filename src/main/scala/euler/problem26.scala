package euler

import scala.collection.mutable._
import scala.math._
import BigInt._
import java.io._
import util.Time._

// A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

// 1/2 =   0.5
// 1/3 =   0.(3)
// 1/4 =   0.25
// 1/5 =   0.2
// 1/6 =   0.1(6)
// 1/7 =   0.(142857)
// 1/8 =   0.125
// 1/9 =   0.(1)
// 1/10  =   0.1
// Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
// Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.object

// for number k: keep checking 10^n % k until we get a repeat. The difference between n1 and n2 = the cycle length.
// if 10^n % k ever == 0, there's no cycle and we move to the next k
object Problem26 {
  def calculateCycle(num: Int): Int = {
    var pow10: BigInt = 10
    var idx = 0
    var moduli = HashMap[BigInt, Int]()
    while (true) {
     if (pow10 >= num) {
        val mod = pow10 % num
        if (mod == 0) {
          return 0
        } else if (moduli.contains(mod)) {
          return idx - moduli(mod)
        } else {
          moduli(mod) = idx
        }
      }
      pow10 *= 10
      idx += 1
    }
    
    return 0
  }
  
  def main(args: Array[String]) = {
    time {
      println("Problem26: " +  (1 until 1000).map(calculateCycle(_)).max)
    }
  }
}