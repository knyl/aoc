package day07

import day06.Direction.{EAST, NORTH, SOUTH, WEST}
import util.{Map2D, Pos, parseMap}

import scala.annotation.tailrec
import scala.io.Source

def pt1(data: List[(BigInt, List[BigInt])]): BigInt =
  data.filter((num, list) => evaluateEquation1(num, list.tail, list.head)).map(_._1).sum

def evaluateEquation1(answer: BigInt, numbers: List[BigInt], acc: BigInt): Boolean =
  if acc > answer then false
  else if numbers.isEmpty then acc == answer
  else
    evaluateEquation1(answer, numbers.tail, acc + numbers.head) ||
      evaluateEquation1(answer, numbers.tail, acc * numbers.head)

def pt2(data: List[(BigInt, List[BigInt])]): BigInt =
  data.filter((num, list) => evaluateEquation2(num, list.tail, list.head)).map(_._1).sum

def evaluateEquation2(answer: BigInt, numbers: List[BigInt], acc: BigInt): Boolean =
  if acc > answer then false
  else if numbers.isEmpty then acc == answer
  else
    evaluateEquation2(answer, numbers.tail, acc + numbers.head) ||
      evaluateEquation2(answer, numbers.tail, acc * numbers.head) ||
      evaluateEquation2(answer, numbers.tail, BigInt(acc.toString + numbers.head.toString))

def parseInput(str: String): (BigInt, List[BigInt]) = str match
  case s"$num: $rest" => (BigInt(num), rest.split(" ").map(BigInt(_)).toList)
  case _ => throw new IllegalArgumentException(s"Invalid input: $str")

@main
def main(): Unit =
  val input = Source.fromResource("day07.txt").getLines().map(_.trim).toList
  val data = input.map(parseInput)
  println(s"pt1: ${pt1(data)}")
  println(s"pt2: ${pt2(data)}")
