package day06

import scala.io.Source

case class Race(time: Long, distance: Long)

def pt1(lines: List[String]): Long =
  val distances = parseInput(lines).map(calculate)
  distances.product

def calculate(race: Race): Long =
  val times = 0L to race.time
  val distances = times.map(getDistance(_, race.time))
  distances.count(_ > race.distance)

def getDistance(time: Long, totalTime: Long): Long =
  time * (totalTime - time)

def parseInput(lines: List[String]): List[Race] =
  val times = lines.head.dropWhile(_ != ' ').split(" ").filter(!_.isBlank).map(_.toLong).toList
  val distances = lines.last.dropWhile(_ != ' ').split(" ").filter(!_.isBlank).map(_.toLong).toList
  times.zip(distances).map(Race.apply)

def pt2(lines: List[String]): Long =
  val race = parseInput2(lines)
  calculate(race)

def parseInput2(lines: List[String]): Race =
  val time = lines.head.dropWhile(_ != ' ').filter(_ != ' ').toLong
  val distance = lines.last.dropWhile(_ != ' ').filter(_ != ' ').toLong
  Race(time, distance)

@main
def main(): Unit =
  val lines = Source.fromResource("day06.txt").getLines().filter(!_.isBlank).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))

val example =
  """
    |Time:      7  15   30
    |Distance:  9  40  200
    |""".stripMargin.split("\n").toList.filter(s => !s.isBlank)
