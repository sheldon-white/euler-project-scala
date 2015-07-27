package euler

import scala.collection.mutable._
import scala.math._
import BigInt._
import java.io._

// Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over
// five-thousand first names, begin by sorting it into alphabetical order.
// Then working out the alphabetical value for each name, multiply this value by its alphabetical position
// in the list to obtain a name score.
// For example, when the list is sorted into alphabetical order, COLIN,
// which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
// So, COLIN would obtain a score of 938 × 53 = 49714.
//What is the total of all the name scores in the file?

object Problem22 {
  
  def readNames() = {
    val stream : InputStream = getClass.getResourceAsStream("p022_names.txt")
    val words = scala.io.Source.fromInputStream(stream).getLines map {_.replace("\"", "")} map {_.split(",") toList}
    (List() ++ words.flatten).sorted
  }
  
  def nameValue(name: String) = {
    name.toList.map {_ - 'A' + 1} sum
  }
  
	def main(args: Array[String]) = {
    val words = readNames()
    var i = 1
    var sum = 0
    for (word <- words) {
      sum += i * nameValue(word)
      i += 1
    }
   println("Problem 22: " + sum)
  }
}
