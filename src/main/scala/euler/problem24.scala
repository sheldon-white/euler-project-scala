package euler

import scala.collection.mutable._
import scala.math._
import BigInt._
import java.io._

// A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of
// the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call
// it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
// 012   021   102   120   201   210
// What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

object Problem24 {
  
  def next(permutation: List[Int]) : List[Int] = {
    val permLength = permutation.length
    var increasing = increasingSequence(permutation)
    increasing.length match {
      case `permLength` => List() // done
      case _ => buildNext(permutation, increasing.length)
    }
  }
  
  def buildNext(permutation: List[Int], increasingSequenceLength: Int): List[Int] = {
    var increasingSequence = permutation.slice(0, increasingSequenceLength)
    var remainder = permutation.slice(increasingSequenceLength + 1, permutation.length)
    var valToCycle = permutation(increasingSequenceLength)
    
    // find next highest value in the increasing sequence
    val nextHighest = permutation.sorted find {_ > valToCycle} get
    var firstPart = ((increasingSequence filter {_ != nextHighest}) :+ valToCycle).sortWith(_ > _) :+ nextHighest
    firstPart ++ remainder
  }
  
  def increasingSequence(permutation: List[Int]) : List[Int] = {
    var i = 0
    while (i < permutation.length - 1 && permutation(i + 1) > permutation(i)) {
      i += 1
    }
    i match {
      case 0 => List(permutation(0))
      case _ => permutation.slice(0, i + 1).toList
    }
  }
 
	def main(args: Array[String]) = {
    //println(decreasingSequence(List(4,3,2,1,0)))
    var seq = List(9,8,7,6,5,4,3,2,1,0)
    for (i <- 0 to 1) {
      seq = next(seq)
      println(seq)
    }
 //  println("Problem 24: " + permutation.reverse)
 // }
  }
}