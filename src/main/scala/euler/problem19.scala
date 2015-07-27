package euler

import scala.collection.mutable._
import scala.collection.immutable._
import scala.math._
import BigInt._

//You are given the following information, but you may prefer to do some research for yourself.
//
//1 Jan 1900 was a Monday.
//Thirty days has September,
//April, June and November.
//All the rest have thirty-one,
//Saving February alone,
//Which has twenty-eight, rain or shine.
//And on leap years, twenty-nine.
//A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
//How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

object Problem19 {
  val monthLengths = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  val lyMonthLengths = List(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);
  
  def isLeapYear(year: Int): Boolean = {
    return year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)
  }
    
	def main(args: Array[String]) = {
    var sundayCount = 0
    var firstDay1900 = 1 // 0 = sunday, 6 = saturday
    var currentDay = (1 + 365) % 7 // first day of 1901 was a tuesday, 1900 wasn't a leap year
    for (year <- 1901 to 2000) {
      var lengths = monthLengths
      if (isLeapYear(year)) {
        lengths = lyMonthLengths
      }
      var mIndex = 1
      for (length <- lengths) {
        mIndex += 1       
        currentDay = (currentDay + length) % 7
        if (currentDay == 0) {
          sundayCount += 1
          //println("SUNDAY")
        }
      }
    }
    println("Problem 19: " + sundayCount)
  }
}
