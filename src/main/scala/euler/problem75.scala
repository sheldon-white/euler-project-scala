package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right angle triangle
// in exactly one way, but there are many more examples.
//
// 12 cm: (3,4,5)
// 24 cm: (6,8,10)
// 30 cm: (5,12,13)
// 36 cm: (9,12,15)
// 40 cm: (8,15,17)
// 48 cm: (12,16,20)
//
// In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle,
// and other lengths allow more than one solution to be found; for example, using 120 cm it is possible
// to form exactly three different integer sided right angle triangles.
//
//120 cm: (30,40,50), (20,48,52), (24,45,51)
//
// Given that L is the length of the wire, for how many values of L ≤ 1,500,000 can exactly
// one integer sided right angle triangle be formed?
object Problem75 {
  def gcd(m: Int, n: Int): Int = {
    val n1 = max(m, n)
    val n2 = min(m, n)
    (n1, n2) match {
      case (a, b) if (a % b == 0) => b
      case _ => gcd(n2, n1 % n2)
    }
  }

  def generateSumsOfTriples = {
    val lengthCounts = HashMap[Long, Int]().withDefaultValue(0)
    val maxSum = 1500000
    val limit = sqrt(maxSum).toInt + 1
    for (n <- 1 to limit; m <- n + 1 to limit) {
      val a = m * m - n * n
      val b = 2 * m * n
      //val c = m * m + n * n
      val sum = 2 * m * (m + n)
      if (gcd(a, b) == 1) {
       // println(m + "," + n + ":", a, b, c)
        var s = sum
        while (s <= maxSum) {
          lengthCounts(s) += 1
          s += sum
        }
      }
    }
    
    lengthCounts
  }
  
  def main(args: Array[String]) = {
    time {
      val lengthCounts = generateSumsOfTriples
      val count = lengthCounts.values.filter(_ == 1).size 
      println("Problem 75: " + count)
    }
  }
}