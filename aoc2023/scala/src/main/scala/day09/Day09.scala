package day09

import scala.annotation.tailrec
import scala.io.Source

def pt1(lines: List[String]): Long =
  lines.map(parseNumbers).map(extrapolate).sum

def parseNumbers(line: String): List[Long] =
  line.split(" ").map(_.toLong).toList

def extrapolate(numbers: List[Long]): Long =
  extrapolate(numbers, numbers.last)

@tailrec
def extrapolate(numbers: List[Long], acc: Long): Long =
  if numbers.forall(_ == 0) then acc
  else
    val differences = numbers.sliding(2).map(l => l(1) - l.head).toList
    extrapolate(differences, acc + differences.last)

def pt2(lines: List[String]): Long =
  lines.map(parseNumbers).map(extrapolate2).sum

def extrapolate2(numbers: List[Long]): Long =
  val differences = differencesUntilZero(numbers, List(numbers))
  extrapolate2(differences)

@tailrec
def differencesUntilZero(value: List[Long], acc: List[List[Long]]): List[List[Long]] =
  val differences = value.sliding(2).map(l => l(1) - l.head).toList
  if differences.forall(_ == 0) then differences :: acc
  else differencesUntilZero(differences, differences :: acc)

@tailrec
def extrapolate2(numbers: List[List[Long]], acc: Long = 0): Long =
  if numbers.isEmpty then acc
  else extrapolate2(numbers.tail, numbers.head.head - acc)

@main
def main(): Unit =
  val lines = Source.fromResource("day09.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
