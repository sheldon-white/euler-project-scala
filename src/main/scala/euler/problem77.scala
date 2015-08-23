package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// It is possible to write ten as the sum of primes in exactly five different ways:
//
// 7 + 3
// 5 + 5
// 5 + 3 + 2
// 3 + 3 + 2 + 2
// 2 + 2 + 2 + 2 + 2
//
// What is the first value which can be written as the sum of primes in over five thousand different ways?
object Problem77 {
  
  def main(args: Array[String]) = {
    time {
      val primes = Vector(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97)
      val firstMatch = (2 to 100) map {n => (n, Partitions.partitionsFromSet(n, primes))} find {_._2 > 5000}
      println("Problem 77: " + firstMatch.get._1)
    }
  }
}