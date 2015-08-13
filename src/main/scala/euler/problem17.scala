package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._
import BigInt._
import util.Time._

// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then
// there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
// how many letters would be used?
// NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
// contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
// The use of "and" when writing out numbers is in compliance with British usage.

object Problem17 {
  val onesWords = Array("", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val teensWords = Array("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen")
  val tensWords = Array("", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")

  def verbiage(num: Int): String = {
    if (num < 1 || num > 9999) {
      throw new IllegalArgumentException("value must be > 0 and < 10000")
    }
    var numberWords = ArrayBuffer[String]()
    val onesPart = num % 10
    val tensPart = (num % 100 - onesPart) / 10
    val hundredsPart = (num % 1000 - (10 * tensPart) - onesPart) / 100
    val thousandsPart = (num - (100 * hundredsPart) - (10 * tensPart) - onesPart) / 1000
    //println(thousandsPart + "," + hundredsPart + ", " + tensPart + ", " + onesPart)
    if (thousandsPart > 0) {
      numberWords += onesWords(thousandsPart)
      numberWords += "thousand"
      if (num % 1000 != 0 ) {
        numberWords += "and"
      }
    }
    if (hundredsPart > 0) {
      numberWords += onesWords(hundredsPart)
      numberWords += "hundred"
      if (num % 100 != 0 ) {
        numberWords += "and"
      }
    }
    if (tensPart == 1) {
      numberWords += teensWords(onesPart)
    } else {
      numberWords += tensWords(tensPart)
      if (onesPart > 0) {
        numberWords += onesWords(onesPart)
      }
    }
    return numberWords.mkString
  }
   
	def main(args: Array[String]) = {
    time {
      val allNumbers = for (i <- 1 to 1000) yield verbiage(i)
      val totalChars = allNumbers.map {_.length}.foldLeft(0) (_ + _)
      println("Problem 17: " + totalChars)
    }
  }
}
