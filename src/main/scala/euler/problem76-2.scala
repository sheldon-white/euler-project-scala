package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// It is possible to write five as a sum in exactly six different ways:
//
// 4 + 1
// 3 + 2
// 3 + 1 + 1
// 2 + 2 + 1
// 2 + 1 + 1 + 1
// 1 + 1 + 1 + 1 + 1
// How many different ways can one hundred be written as a sum of at least two positive integers?

// p(k) = p(k − 1) + p(k − 2) − p(k − 5) − p(k − 7) + p(k − 12) + p(k − 15) − p(k − 22) − ...
// where (1, 2, 5, 7, 12) = 1/2n(3n-1), n = ±1, ±2, ±3, ...

// or: p(n) = Σ (-1)^k+1[P(n - k(3k-1)/2) + P(n - k(3k+1)/2)], where k(3k+1)/2 >= n
// p(10) = 42
// p(20) = 627
// p(30) = 5604

object Problem76_2 {
  
  def main(args: Array[String]) = {
    time {
      println("Problem 76: " + (Partitions.partitionsFromIntegers(100) - 1))
    }
  }
}