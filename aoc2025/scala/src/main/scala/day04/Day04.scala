package day04

import util.{Map2D, Pos, parseMap, printMap}

import scala.annotation.tailrec
import scala.io.Source

extension (map: Map2D[Char])
  def neighbours(pos: Pos): List[Pos] =
    List(
      Pos(pos.x - 1, pos.y - 1), Pos(pos.x, pos.y - 1), Pos(pos.x + 1, pos.y - 1),
      Pos(pos.x - 1, pos.y), Pos(pos.x + 1, pos.y),
      Pos(pos.x - 1, pos.y + 1), Pos(pos.x, pos.y + 1), Pos(pos.x + 1, pos.y + 1)
    )

def pt1(map: Map2D[Char]): Int =
  map.allKeys.count(pos => canMoveRolls(map, pos))

def canMoveRolls(map: Map2D[Char], pos: Pos): Boolean =
  val neighbours = map.neighbours(pos)
  map(pos) == '@' && neighbours.count(map(_) == '@') < 4

@tailrec
def pt2(map: Map2D[Char], removedCount: Int = 0): Int =
  val (updatedMap, count) = removeRolls(map)
  if count == 0 then removedCount
  else pt2(updatedMap, removedCount + count)


def removeRolls(map: Map2D[Char]): (Map2D[Char], Int) =
  val removedRolls = map.allKeys.foldLeft(Nil : List[Pos])((removedRolls, pos)  =>
    if canMoveRolls(map, pos) then pos :: removedRolls
    else removedRolls
  )
  (updateMap(map, removedRolls), removedRolls.length)

def updateMap(map: Map2D[Char], removedRolls: List[Pos]): Map2D[Char] =
  removedRolls.foldLeft(map)((m, pos) => m.update(pos, '.'))

@main
def main(): Unit =
  val input = Source.fromResource("day04.txt").getLines().toList
  val map = parseMap(input, identity, '.')

  println("Pt1: " + pt1(map))
  println("Pt2: " + pt2(map))

val example =
  """
    |..@@.@@@@.
    |@@@.@.@.@@
    |@@@@@.@.@@
    |@.@@@@..@.
    |@@.@@@@.@@
    |.@@@@@@@.@
    |.@.@.@.@@@
    |@.@@@.@@@@
    |.@@@@@@@@.
    |@.@.@@@.@.
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList
