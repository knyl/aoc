package day09

import scala.annotation.tailrec
import scala.io.Source

@tailrec
def pt1(data: List[Option[Int]], compacted: List[Int] = List()): BigInt =
  if data.isEmpty then checksum(compacted)
  else (data.head, data.last) match
    case (Some(ind), Some(_)) => pt1(data.tail, compacted :+ ind)
    case (Some(_), None)      => pt1(data.dropRight(1), compacted)
    case (None, Some(ind))    => pt1(data.tail.dropRight(1), compacted :+ ind)
    case (None, None)         => pt1(data.dropRight(1), compacted)

def checksum(data: List[Int]): BigInt =
  data.zipWithIndex.map((num, index) => BigInt(num) * index).sum

@tailrec
def parseInput(str: String, data: List[Option[Int]] = List(), currentIndex: Int = 0): List[Option[Int]] =
  if str.isEmpty then data
  else if str.length == 1 then
    val fileLength = str.head.asDigit
    val fileBlock = (0 until fileLength).map(_ => Option(currentIndex)).toList
    data ++ fileBlock
  else
    val fileLength = str.head.asDigit
    val fileBlock = (0 until fileLength).map(_ => Option(currentIndex)).toList
    val emptySpacesLength = str.tail.head.asDigit
    val emptySpaces = (0 until emptySpacesLength).map(_ => Option.empty[Int])
    parseInput(str.tail.tail, data ++ fileBlock ++ emptySpaces, currentIndex + 1)

@tailrec
def parseInput2(str: String, data: List[(Int, Option[Int])] = List(), currentIndex: Int = 0): List[(Int, Option[Int])] =
  if str.isEmpty then data
  else if str.length == 1 then
    val fileLength = str.head.asDigit
    data :+ (fileLength, Option(currentIndex))
  else
    val fileLength = str.head.asDigit
    val fileBlock = (fileLength, Option(currentIndex))
    val emptySpacesLength = str.tail.head.asDigit
    val emptySpaces = (emptySpacesLength, Option(currentIndex + 1))
    parseInput2(str.tail.tail, data :+ fileBlock :+ emptySpaces)

@main
def main(): Unit =
  val input = Source.fromResource("day09.txt").mkString
  val data = parseInput(input)
  println(s"pt1: ${pt1(data)}")
  println(s"pt2: ${}")

val example = "12345"

val example2 = "2333133121414131402"
