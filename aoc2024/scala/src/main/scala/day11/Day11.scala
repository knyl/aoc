package day11

import scala.annotation.tailrec
import scala.io.Source

@tailrec
def pt1(stones: List[String], blinks: Int): Int =
  if blinks == 0 then stones.length
  else
    val updatedStones: List[String] = stones.flatMap {
      case "0" => List("1")
      case s if s.length % 2 == 0 =>
        val (s1, s2) = s.splitAt(s.length / 2)
        List(s1, BigInt(s2).toString)
      case s => List((BigInt(s) * 2024).toString)
    }
    pt1(updatedStones, blinks - 1)

@tailrec
def pt2(stones: Map[String, BigInt], blinks: Int): BigInt =
  if blinks == 0 then stones.values.sum
  else
    val updatedStones = stones.toList.flatMap {
      case (stone, count) =>
        stone match
          case "0" =>
            List(("1", count))
          case s if s.length % 2 == 0 =>
            val (s1, s2) = s.splitAt(s.length / 2)
            List((s1, count), (BigInt(s2).toString, count))
          case s =>
            List(((BigInt(s) * 2024).toString, count))
    }
    val updatedMap = updatedStones.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
    pt2(updatedMap, blinks - 1)

@main
def main(): Unit =
  val stones = Source.fromResource("day11.txt").mkString.split(" ").toList
  println(s"pt1: ${pt1(stones, 25)}")
  val stoneMap = stones.groupBy(identity).view.mapValues(v => BigInt(v.length)).toMap
  println(s"pt2: ${pt2(stoneMap, 75)}")
