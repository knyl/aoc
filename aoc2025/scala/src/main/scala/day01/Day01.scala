package day01

import scala.io.Source

def pt1(list1: List[String]): Int =
  val result = list1.foldLeft((50, 0))((acc, curr) =>
    val direction = curr.charAt(0)
    val rotations = curr.substring(1).toInt
    val updatedValue = direction match
      case 'L' => (acc._1 - rotations) % 100
      case 'R' => (acc._1 + rotations) % 100
      case _   => throw new IllegalArgumentException("Invalid direction: " + curr)
    val isZero = if updatedValue == 0 then 1 else 0
    (updatedValue, acc._2 + isZero)
  )
  result._2

def pt2(list: List[String]): Int =
  val result = list.foldLeft((50, 0))((acc, curr) =>
    val direction = curr.charAt(0)
    val rotations = curr.substring(1).toInt
    val rotatingZero = rotations / 100
    val limitRotations = rotations % 100
    val updatedValue = direction match
      case 'L' => (acc._1 - limitRotations + 100) % 100
      case 'R' => (acc._1 + limitRotations) % 100
      case _   => throw new IllegalArgumentException(s"Invalid input: $curr")
    val passedZero = direction match
      case 'L' => if acc._1 != 0 && acc._1 - limitRotations < 0 then 1 else 0
      case 'R' => if acc._1 != 0 && acc._1 + limitRotations > 100 then 1 else 0
      case _   => throw new IllegalArgumentException(s"Invalid input: $curr")
    val isZero = if updatedValue == 0 && acc._1 != 0 then 1 else 0
    (updatedValue, acc._2 + isZero + rotatingZero + passedZero)
  )
  result._2

@main
def main(): Unit =
  val input = Source.fromResource("day01.txt").getLines().map(_.trim).toList

  println("Pt1: " + pt1(input))
  println("Pt2: " + pt2(input))
