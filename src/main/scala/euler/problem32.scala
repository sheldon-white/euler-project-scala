package euler

import scala.collection.mutable._
import scala.math._

// We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once;
// for example, the 5-digit number, 15234, is 1 through 5 pandigital.
// The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier,
// and product is 1 through 9 pandigital.
// Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
// HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.

// The only legal multipler sizes are: AA x BBB = CCCC and A x BBBB = CCCC
// so we can limit the splits to 1,4,5 and 2,3,4
// 1) loop through all permutations
// 2) try the two splits
object Problem32 {
  val permutations = List(1,2,3,4,5,6,7,8,9).permutations
  val matchingProducts = HashSet[Int]()
  
  def case1(permutation: List[Int]) = {
      val s1 = permutation.slice(0, 1).map {x => (x + '0').toChar} mkString
      val s2 = permutation.slice(1, 5) map {x => (x + '0').toChar} mkString
      val s3 = permutation.slice(5, 9) map {x => (x + '0').toChar} mkString
      val m1 = s1.mkString.toInt
      val m2 = s2.mkString.toInt
      val product = s3.mkString.toInt
      if (m1 * m2 == product) {
        matchingProducts.add(product)
      }    
  }
  
  def case2(permutation: List[Int]) = {
      val s1 = permutation.slice(0, 2).map {x => (x + '0').toChar} mkString
      val s2 = permutation.slice(2, 5) map {x => (x + '0').toChar} mkString
      val s3 = permutation.slice(5, 9) map {x => (x + '0').toChar} mkString
      val m1 = s1.mkString.toInt
      val m2 = s2.mkString.toInt
      val product = s3.mkString.toInt
      if (m1 * m2 == product) {
        matchingProducts.add(product)
      }    
  }
  
  def main(args: Array[String]) = {
    for (permutation <- permutations) {
      case1(permutation)
      case2(permutation)
    }
    println("Problem 32: " + matchingProducts.sum)    
  }
}
