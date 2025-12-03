package day03

import scala.io.Source

def solve(batteryBanks: List[String], levels: Int): Long =
  val positions = levels - 1 to 0 by -1
  batteryBanks
    .map { batteryBank =>
      val voltageList = positions.foldLeft((List[Long](), 0))((acc, pos) =>
        findMaxVoltage(batteryBank)(acc, pos)
      )._1
      voltageList.reverse.map(_.toString).mkString.toLong
    }.sum

def findMaxVoltage(batteryBank: String)(acc: (List[Long], Int), pos: Int): (List[Long], Int) =
  val (voltageList, index) = acc
  val (maxVoltage, maxIndex) = batteryBank
    .substring(index, batteryBank.length - pos)
    .map(_.asDigit.toLong)
    .zipWithIndex
    .maxBy(_._1)
  (maxVoltage :: voltageList, maxIndex + index + 1)

@main
def main(): Unit =
  val input = Source.fromResource("day03.txt").getLines().toList

  println("Pt1: " + solve(input, 2))
  println("Pt2: " + solve(input, 12))

val example =
  """
    |987654321111111
    |811111111111119
    |234234234234278
    |818181911112111
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList
