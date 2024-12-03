package day03

import scala.io.Source


def pt1(input: String): Int =
  "mul\\((\\d+,\\d+)\\)".r
    .findAllMatchIn(input)
    .map(_.group(1))
    .map(_.split(",").map(_.toInt))
    .map(a => a(0) * a(1))
    .sum

def pt2(input: String): Int =
  input
    .split("(?=do\\(\\)|don't\\(\\))")
    .filterNot(_.contains("don't()"))
    .map(pt1)
    .sum


@main
def main(): Unit =
  val input = Source.fromResource("day03.txt").getLines().map(_.trim).mkString("")
  println(s"pt1: ${pt1(input)}")
  println(s"pt2: ${pt2(input)}")
