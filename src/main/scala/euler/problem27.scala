package euler

import scala.collection.mutable._
import scala.math._
import BigInt._
import java.io._
import util.Prime

// Considering quadratics of the form:
// n^2 + an + b, where |a| < 1000 and |b| < 1000
// where |n| is the modulus/absolute value of n
// e.g. |11| = 11 and |−4| = 4
// Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

object Problem27 {

  def primeSequence(a: Int, b: Int): Int = {
    var n = 0;
    while (Prime.isPrime(abs(n*n + a*n + b))) {
      n += 1
      //println(n*n + a*n + b + " prime")
    }
    n
  }
  
  def main(args: Array[String]) = {
    var aMax = 0;
    var bMax = 0;
    var seqMax = 0
    for (a <- -1000 to 1000; b <- -1000 to 1000) {
      //println(a + "," + b)
      if (Prime.isPrime(b)) {
        val seqLen = primeSequence(a, b)
        if (seqLen > seqMax) {
          seqMax = seqLen
          aMax = a
          bMax = b
          //println(seqMax + "," + a + "," + b + " = " + a*b)
        }
      }
    }
    println("Problem27: " + aMax*bMax)
  }
}