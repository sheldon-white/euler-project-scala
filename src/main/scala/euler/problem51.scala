package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._

// By replacing the 1st digit of the 2-digit number *3, it turns out that
// six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.
// By replacing the 3rd and 4th digits of 56**3 with the same digit,
// this 5-digit number is the first example having seven primes among the ten generated numbers,
// yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
// Consequently 56003, being the first member of this family, is the smallest prime with this property.
// Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits)
// with the same digit, is part of an eight prime value family.

// We can't replace the rightmost digit.
// Traverse all combinations of digits except rightmost (all binary numbers with N-1 digits)
// for each, replace in turn the digits marked by 1's with 0-9, see which are prime
object Problem51 {
  val permutations = HashMap[Int, Vector[Vector[Char]]]()
  
  def binaryPermutations(len: Int) = {
    def format(v: Int) = String.format("%" + len + "s", Integer.toBinaryString(v)).replace(' ', '0').toVector;
    
    if (!permutations.contains(len)) {
      val maxVal = (pow(2, len + 1) - 1).toInt
      permutations(len) = ((1 to maxVal).map(n => format(n)).toVector)
    }
    permutations(len)
  }

  def replaceDigits(value: Int, digit: Char, permutation: Vector[Char]) = {
    val valueDigits = value.toString.toList
    var newValue = ArrayBuffer[Char]()
    for (i <- 0 until valueDigits.length - 1) {
      permutation(i) match {
        case '1' => newValue += digit
        case '0' => newValue += valueDigits(i)
      }
    }
    newValue += valueDigits.last
    newValue.mkString("").toInt
  }
  
  def primeReplacementsForPermutation(value: Int, permutation: Vector[Char]) = {
    ('0' to '9') map {replaceDigits(value, _, permutation)} filter {Prime.isPrime(_)}
  }

  def primeReplacements(value: Int) = {
    val permutations = binaryPermutations(value.toString.toList.length - 1)
    permutations map {primeReplacementsForPermutation(value, _)}
  }
  

  def main(args: Array[String]) = {
    time {
      breakable {
        for (i <- 10 to 1000000; if Prime.isPrime(i)) {
          val primeSets = primeReplacements(i)
          for (set <- primeSets; if set.length >= 8; if set.forall(p => p.toString.length == set(0).toString.length)) {
            println("Problem 51: " + set.min)
            break
          }
        }
      }
    }
  }
}
