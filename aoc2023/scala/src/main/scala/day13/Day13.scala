package day13

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

def pt1(lines: List[String]): Int =
  val maps = parseInput(lines)
  maps.map(findScore).sum

def pt2(lines: List[String]): Int =
  val maps = parseInput(lines)
  maps.map(findScore2).sum

def findScore(map: List[String]): Int =
  val horizontal = findMirror(map).headOption.map(_ + 1).map(_ * 100)
  val vertical = findMirror(map.transpose.map(_.mkString)).headOption.map(_ + 1)
  horizontal.orElse(vertical).getOrElse(-1)

def findScore2(map: List[String]): Int =
  val horizontal = findSmudgedReflection(map).map(_ * 100)
  val vertical = findSmudgedReflection(map.transpose.map(_.mkString))
  horizontal.orElse(vertical).getOrElse(-1)

def isMirror(map: List[String], i: Int): Boolean =
  val (l1, l2) = map.splitAt(i + 1)
  (l1.reverse zip l2).forall((l1, l2) => l1 == l2)

def findMirror(map: List[String]) =
  map.zipWithIndex.sliding(2)
    .filter(l => l.head._1 == l.last._1)
    .map(_.head._2)
    .filter(isMirror(map, _))
    .toList

def findSmudgedReflection(map: List[String]): Option[Int] =
  val originalMirror = findMirror(map).headOption
  val indices = for {i <- map.indices
                     j <- map.head.indices
                     } yield (i, j)
  hasSmudgedReflection(indices.toList, originalMirror, map)

@tailrec
def hasSmudgedReflection(positions: List[(Int, Int)], originalMirror: Option[Int], map: List[String]): Option[Int] =
  positions.headOption match
    case None => None
    case Some(position) =>
      val (i, j) = position
      val newMap =
        if map(i)(j) == '#' then map.updated(i, map(i).updated(j, '.'))
        else map.updated(i, map(i).updated(j, '#'))
      val newMirror = findMirror(newMap)
      if newMirror.nonEmpty && originalMirror.isEmpty then Some(newMirror.head + 1)
      else if newMirror.nonEmpty && newMirror.exists(_ != originalMirror.get) then
        val findMirror = newMirror.filter(_ != originalMirror.get)
        Some(findMirror.head + 1)
      else hasSmudgedReflection(positions.tail, originalMirror, map)


@tailrec
def parseInput(lines: List[String], acc: List[String] = List(), result: List[List[String]] = List()): List[List[String]] = lines.headOption match
  case None => (acc.reverse :: result).filter(_.nonEmpty).reverse
  case Some(line) if line.isBlank => parseInput(lines.tail, List(), acc.reverse :: result)
  case Some(line) => parseInput(lines.tail, line :: acc, result)

@main
def main(): Unit =
  val lines = Source.fromResource("day13.txt").getLines().toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
