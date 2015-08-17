package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order
// the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime.
// The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
// Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.

object Problem60 {
  val maxPrime = 10000
  val smallPrimes = Prime.primes.takeWhile(_ < maxPrime) drop(3) toVector
  val splitMod3 = smallPrimes partition {p => p % 3 == 1}
  val primes1 = 3L +: splitMod3._1
  val primes2 = 3L +: splitMod3._2

  def allPrime(p1: Long, p2: Long): Boolean = {
    val p1Str = p1.toString
    val p2Str = p2.toString
    val c1 = p1Str.concat(p2Str).toLong
    val c2 = p2Str.concat(p1Str).toLong
    if (!Prime.isPrime(c1)) {
      return false
    }
    if (!Prime.isPrime(c2)) {
      return false
    }
    return true
  }

  def findEm(primes: Vector[Long]) = {
     breakable {
       var i1 = 0
       while (i1 < primes.size) {
         val p1 = primes(i1)
         //println(p1)
         var i2 = i1 + 1
         i1 += 1
         while (i2 < primes.size) {
           val p2 = primes(i2)
           var i3 = i2 + 1
           i2 += 1
           if (allPrime(p1, p2)) { 
             //println(p1, p2)
             while (i3 < primes.size) {
               val p3 = primes(i3)
               var i4 = i3 + 1
               i3 += 1
               if (allPrime(p1, p3) && allPrime(p2, p3)) { 
                 //println(p1, p2, p3)
                 while (i4 < primes.size) {
                   val p4 = primes(i4)
                   var i5 = i4 + 1
                   i4 += 1
                   if (allPrime(p1, p4) && allPrime(p2, p4) && allPrime(p3, p4)) { 
                      while (i5 < primes.size) {
                       val p5 = primes(i5)
                       i5 += 1
                       if (allPrime(p1, p5) && allPrime(p2, p5) && allPrime(p3, p5) && allPrime(p4, p5)) { 
                         val s = List(p1,p2,p3,p4,p5).sum
                         println("Problem 60: " + s)
                         break
                       }
                     }
                   }
                 }
               }
             }
           }
         }
       }
     }    
  }
  
  
  def main(args: Array[String]) = {
    time {
      breakable {
        for (group <- Vector(primes1 , primes2)) {
          findEm(group)
        }
      }
    }
  }
}
