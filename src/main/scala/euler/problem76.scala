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

object Problem76 {
    // key looks like "100-50": size-largestUnit
  var subCounts: Map[String, Int] = HashMap[String, Int]()
  
  def partitions(size: Int, units: Vector[Int]): Int = {
    val key = size + "-" + units(0)
    //println("partitioning " + size + " with " + units)
    if (subCounts.contains(key)) {
      return subCounts(key)
    } else {
      var count = 0
      var unitSlice = units
      for (unit <- units) {
        if (size > unit) {
          count += partitions(size - unit, unitSlice)
        } else if (size == unit) {
          count += 1
        }
        unitSlice = unitSlice.drop(1)
      }
      subCounts(key) = count
      //println(key + " = " + count)
      return count
    } 
  }

  def main(args: Array[String]) = {
    time {
      val size = 100
      val units = Vector.tabulate(size - 1)(_ + 1)
      println("Problem 76: " + partitions(size, units))
    }
  }
}