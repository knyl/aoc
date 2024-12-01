package day01

import scala.io.Source

def pt1(list1: List[Int], list2: List[Int]): Int =
  val zippedList = list1.zip(list2)
  zippedList.map((a, b) => b - a).map(_.abs).sum

def pt2(list1: List[Int], list2: List[Int]): Int =
  list1.map(n => list2.count(_ == n) * n).sum

def parseInput(line: List[String]): (List[Int], List[Int]) =
  val lines = line.map(_.split("\\s+").map(_.toInt).toList)
  val list1 = lines.flatMap(_.headOption).sorted
  val list2 = lines.flatMap(_.tail.headOption).sorted
  (list1, list2)

@main
def main(): Unit =
  val input = Source.fromResource("day01.txt").getLines().map(_.trim).toList
  val (list1, list2) = parseInput(input)

  println("Pt1: " + pt1(list1, list2))
  println("Pt2: " + pt2(list1, list2))

val example =
  """
    |3   4
    |4   3
    |2   5
    |1   3
    |3   9
    |3   3
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList
