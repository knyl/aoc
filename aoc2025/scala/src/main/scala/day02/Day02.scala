package day02

import scala.io.Source

def findRepeatPt1(num: Long): BigInt =
  val str = num.toString
  if str.length % 2 != 0 then 0
  else
    val (firstHalf, secondHalf) = str.splitAt(str.length / 2)
    if firstHalf == secondHalf then BigInt(num) else BigInt(0)

def findRepeatPt2(num: Long): BigInt =
  val str = num.toString
  val (firstHalf, _) = str.splitAt(str.length / 2)
  val allSubstrings = (1 to firstHalf.length).map(i => firstHalf.substring(0, i)).toList
  allSubstrings.map(s => findRepeatInString(s, str)).toSet.sum

def findRepeatInString(repeat: String, str: String): BigInt =
  val pattern = ("^(" + repeat + ")+$").r
  pattern.findFirstIn(str) match
    case Some(matched) => BigInt(str)
    case None => BigInt(0)

def solve(list: List[(Long, Long)], solveFun: Long => BigInt): BigInt =
  list.map(range => findRepeats(range, solveFun)).sum

def findRepeats(range: (Long, Long), solveFun: Long => BigInt): BigInt =
  val (start, end) = range
  (start to end).map(solveFun).sum

def parseInput(strings: List[String]): List[(Long, Long)] =
  strings.map(str => {
    val strList = str.split('-')
    (strList(0).toLong, strList(1).toLong)
  } )

@main
def main(): Unit =
  val input = Source.fromResource("day02.txt").getLines().map(_.trim).toList.head.split(",").toList

  println("Pt1: " + solve(parseInput(input), findRepeatPt1))
  println("Pt2: " + solve(parseInput(input), findRepeatPt2))

val example =
  """
    |11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124
    |""".stripMargin.split(",").map(_.trim).filterNot(_.isBlank).toList

val example2 =
  """
    |111-111
    |""".stripMargin.split(",").map(_.trim).filterNot(_.isBlank).toList
