package euler

import scala.collection.mutable._
import scala.math._
import util.Prime
import java.io._
import util.Time._
import scala.util.control.Breaks._
import scala.collection.breakOut   

// A common security method used for online banking is to ask the user for three random characters from a passcode.
// For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters;
// the expected reply would be: 317.
//
// The text file, keylog.txt, contains fifty successful login attempts.
//
// Given that the three characters are always asked for in order,
// analyse the file so as to determine the shortest possible secret passcode of unknown length.

// This is a problem in flattening a graph to the shortest possible form.
// 2 -> Set(0, 8, 9)
// 8 -> Set(0, 9)
// 7 -> Set(3, 1, 2, 9, 6)
// 1 -> Set(0, 8, 2, 9, 6),
// 6 -> Set(0, 8, 2, 9)
// 9 -> Set(0)
// 3 -> Set(1, 8, 6))

// Maybe just start inserting values into a list, based on what constraints they have with other elements already inserted.

object Problem79 {
  def readSequences() = {
    val stream : InputStream = getClass.getResourceAsStream("p079_keylog.txt")
    val sequences = scala.io.Source.fromInputStream(stream).getLines
    sequences.toVector.distinct
  }

  def generateAllOrderings(sequences: Vector[String]) = {
    val orderings = HashMap[Char, Set[Char]]()
    for (seq <- sequences) {
      val c = seq.toVector
      val c0 = c(0)
      val c1 = c(1)
      val c2 = c(2)
      if (!orderings.contains(c0)) {
        orderings(c0) = HashSet[Char]()
      }
      if (!orderings.contains(c1)) {
        orderings(c1) = HashSet[Char]()
      }
      val o0 = orderings(c0)
      o0.add(c1)
      o0.add(c1)
      //println(o0)
      orderings(c0) = o0
      val o1 = orderings(c1)
      o1.add(c2)
      //println(o1)
      orderings(c1) = o1
    }
    
    orderings
  }
  
  def main(args: Array[String]) = {
    time {
      val sequences = readSequences
      val orderings = generateAllOrderings(sequences)
      println(orderings)
      // LAME. Look at the orderings, easily determine by hand that the answer is...
      // better would be to calculate minimal string. Maybe later.
      println("Problem 79: " + "73162890".length)
    }
  }
}