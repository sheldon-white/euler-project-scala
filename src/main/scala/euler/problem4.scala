package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._

// Find the largest palindrome made from the product of two 3-digit numbers.
object Problem4 {
	def palindromes = for (x <- (100 to 1000);
			y <- (x to 1000);
			val prod = x * y if isPalindrome(prod.toString))
		yield prod

		def isPalindrome(s: String): Boolean = s.reverse.mkString == s

		def max = palindromes.toList.sortWith(_ > _).head

		def main(args: Array[String]) = {
				println("Problem 4: " + max)    
	}
}
