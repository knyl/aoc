package day09

import scala.annotation.tailrec
import scala.io.Source

trait Block:
  val start: Int
  val length: Int

case class File(start: Int, length: Int, index: Int) extends Block

case class FreeSpace(start: Int, length: Int) extends Block

@tailrec
def pt1(data: List[Option[Int]], compacted: List[Int] = List()): BigInt =
  if data.isEmpty then checksum(compacted)
  else (data.head, data.last) match
    case (Some(ind), Some(_)) => pt1(data.tail, compacted :+ ind)
    case (Some(_), None) => pt1(data.dropRight(1), compacted)
    case (None, Some(ind)) => pt1(data.tail.dropRight(1), compacted :+ ind)
    case (None, None) => pt1(data.dropRight(1), compacted)

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

def pt2(input: String): BigInt =
  val (files, freeBlocks) = parseInput2(input)
  val compactedBlocks = compactBlocks(files.reverse, freeBlocks)
  checksum2(compactedBlocks)

def checksum2(files: List[File]): BigInt =
  files.flatMap(f =>
      (f.start until (f.start + f.length))
        .map(i => i * f.index)
        .map(BigInt(_)))
    .sum

@tailrec
def compactBlocks(files: List[File], freeSpace: List[FreeSpace], result: List[File] = List()): List[File] =
  if files.isEmpty then result
  else
    val nextFile = files.head
    getFreeBlock(nextFile, freeSpace) match
      case None => compactBlocks(files.tail, freeSpace, result :+ nextFile)
      case Some((before, free, after)) =>
        if free.length == nextFile.length then
          compactBlocks(files.tail, before ++ after, result :+ File(start = free.start, length = nextFile.length, index = nextFile.index))
        else
          val newFreeSpace = FreeSpace(start = free.start + nextFile.length, length = free.length - nextFile.length)
          compactBlocks(files.tail, before ++ List(newFreeSpace) ++ after, result :+ File(start = free.start, length = nextFile.length, index = nextFile.index))

def getFreeBlock(file: File, spaces: List[FreeSpace]): Option[(List[FreeSpace], FreeSpace, List[FreeSpace])] =
  spaces.find(f => f.length >= file.length && f.start < file.start) match
    case None => None
    case Some(free) =>
      val untilFree = spaces.takeWhile(_ != free)
      val afterFree = spaces.dropWhile(_ != free).tail
      Some(untilFree, free, afterFree)

@tailrec
def parseInput2(str: String, files: List[File] = List(), freeSpace: List[FreeSpace] = List(), fileIndex: Int = 0, currentPos: Int = 0): (List[File], List[FreeSpace]) =
  if str.isEmpty then (files, freeSpace)
  else if str.length == 1 then
    val fileLength = str.head.asDigit
    (files :+ File(start = currentPos, length = fileLength, index = fileIndex), freeSpace)
  else
    val fileLength = str.head.asDigit
    val fileBlock = File(start = currentPos, length = fileLength, index = fileIndex)
    val emptySpacesLength = str.tail.head.asDigit
    val emptyBlock = FreeSpace(start = currentPos + fileLength, length = emptySpacesLength)
    parseInput2(str.drop(2), files :+ fileBlock, freeSpace :+ emptyBlock, fileIndex + 1, currentPos + fileLength + emptySpacesLength)

@main
def main(): Unit =
  val input = Source.fromResource("day09.txt").mkString
  val data = parseInput(input)
  println(s"pt1: ${pt1(data)}")
  println(s"pt2: ${pt2(input)}")

