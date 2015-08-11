package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._

// The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:
// 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
// By converting each letter in a word to a number corresponding to its alphabetical position
// and adding these values we form a word value.
// For example, the word value for SKY is 19 + 11 + 25 = 55 = t10.
// If the word value is a triangle number then we shall call the word a triangle word.
// Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing
// nearly two-thousand common English words, how many are triangle words?
object Problem42 {
  def readWords() = {
    val stream : InputStream = getClass.getResourceAsStream("p042_words.txt")
    val words = scala.io.Source.fromInputStream(stream).getLines map {_.replace("\"", "")} map {_.split(",") toList}
    (List() ++ words.flatten)
  }

  def isTriangular(num: Int) = {
    val root = (-1 + sqrt(1 + 8*num)) / 2
    floor(root) == root
  }
  
  def wordSum(word: String) = {
    word.toList.map(_ - 'A' + 1).sum
  }
  

  
  def main(args: Array[String]) = {
    val words = readWords()
    val triangleWords = words filter {word => isTriangular(wordSum(word))}
    println("Problem 42: " + triangleWords.length)
  }
}
