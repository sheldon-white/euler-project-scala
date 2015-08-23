package euler

import scala.collection.mutable._
import scala.math._
import util.Time._

object Partitions {

  def partitionsFromSet(size: Int, units: Vector[Int]): Int = {
    var subCounts: Map[String, Int] = HashMap[String, Int]()
    _partitionsFromSet(size, units, subCounts)
  }
  
  private def _partitionsFromSet(size: Int, units: Vector[Int], subCounts: Map[String, Int]): Int = {
    val key = size + "-" + units(0)
    //println("partitioning " + size + " with " + units)
    if (subCounts.contains(key)) {
      return subCounts(key)
    } else {
      var count = 0
      var unitSlice = units
      for (unit <- units) {
        if (size > unit) {
          count += _partitionsFromSet(size - unit, unitSlice, subCounts)
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
  

  def partitionsFromIntegers(n: Int): BigInt = {
     var knownPartitions = HashMap[Int, BigInt]()
     partitionsFromIntegers(n, knownPartitions)
  }
  
  //
  // FIXME stack blowout
  //
  def partitionsFromIntegers(n: Int, knownPartitions: Map[Int, BigInt]): BigInt = {
    def _m1(k: Int) = k*(3*k - 1) / 2
    def _m2(k: Int) = k*(3*k + 1) / 2
    println(n)
    if (n < 2) {
      return 1
    }
    if (knownPartitions.contains(n)) {
      return knownPartitions(n)
    }
    var sum = BigInt(0)
    var sign = 1
    var done = false
    var k = 1
    for (k <- 1 to n; if (n - _m1(k) >= 0)) {
      val m1 = n - _m1(k)
      val m2 = n - _m2(k)
      if (m1 >= 0) {
        sum += sign * partitionsFromIntegers(m1, knownPartitions)
      }
      if (m2 >= 0) {
        sum += sign * partitionsFromIntegers(m2, knownPartitions)
      }
      sign *= -1
    }
    knownPartitions(n) = sum
    sum
  }

}
