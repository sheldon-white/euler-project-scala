package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// Let p(n) represent the number of different ways in which n coins can be separated into piles. For example, five coins can be separated into piles in exactly seven different ways, so p(5)=7.
//
// OOOOO
// OOOO   O
// OOO   OO
// OOO   O   O
// OO   OO   O
// OO   O   O   O
// O   O   O   O   O
//Find the least value of n for which p(n) is divisible by one million.
object Problem78 {
  
  //
  // This can work if we already have the partitions for smaller numbers
  //
  def partitionsFromIntegers(n: Long, knownPartitions: Map[Long, BigInt]): BigInt = {
    def _m1(k: Long) = k*(3*k - 1) / 2
    def _m2(k: Long) = k*(3*k + 1) / 2

    if (n < 2) {
      knownPartitions(n) = 1
      return 1
    }
    if (knownPartitions.contains(n)) {
      return knownPartitions(n)
    }
    var sum = BigInt(0)
    var sign = 1
    var done = false
    var k = 1L
    for (k <- 1L to n; if (n - _m1(k) >= 0)) {
      val m1 = n - _m1(k)
      val m2 = n - _m2(k)
      if (m1 >= 0) {
        sum += sign * knownPartitions(m1) 
      }
      if (m2 >= 0) {
        sum += sign * knownPartitions(m2)
      }
      sign *= -1
    }
    knownPartitions(n) = sum
    sum
  }

  def main(args: Array[String]) = {
    time {
      breakable {
        var knownPartitions = HashMap[Long, BigInt]()
        knownPartitions(0) = 1
        knownPartitions(1) = 1
        
        for (n <- 1L to 100000L) {
          val p = partitionsFromIntegers(n, knownPartitions)
          //println(n, p)
          if (p % 1000000 == 0) {
            println("Problem 78: " + n)
            break
          }
        }
      }
    }
  }
}