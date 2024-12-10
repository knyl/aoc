package day10

import util.{Map2D, Pos, parseMap}

import scala.annotation.tailrec
import scala.io.Source

extension (map: Map2D[Char])
  def neighbours(pos: Pos): List[Pos] =
    val currentValue = map(pos)
    List(
      Pos(pos.x, pos.y - 1),
      Pos(pos.x + 1, pos.y),
      Pos(pos.x, pos.y + 1),
      Pos(pos.x - 1, pos.y)
    ).filterNot(map.isOutOfBounds).filter(map(_) == (currentValue + 1))


def pt1(map: Map2D[Char]): Int =
  val trailheads = findTrailheads(map).toList
  trailheads
    .map(p => visit(map, List(p)))
    .map(visited => visited.filter(map(_) == '9'))
    .map(_.size)
    .sum

@tailrec
def visit(map: Map2D[Char], toVisit: List[Pos], visited: Set[Pos] = Set()): Set[Pos] = toVisit match
  case Nil => visited
  case next :: rest =>
    val neighbours = map.neighbours(next).filterNot(visited.contains)
    visit(map, rest ++ neighbours, visited + next)

def findTrailheads(value: Map2D[Char]): Set[Pos] =
  value.map.filter(_._2 == '0').keys.toSet

def pt2(map: Map2D[Char]): Int =
  val trailheads = findTrailheads(map).toList
  trailheads
    .map(visit2(map, _))
    .sum

def visit2(map: Map2D[Char], toVisit: Pos): Int =
  if map(toVisit) == '9' then 1
  else
    val neighbours = map.neighbours(toVisit)
    if neighbours.isEmpty then 0
    else neighbours.map(visit2(map, _)).sum

@main
def main(): Unit =
  val input = Source.fromResource("day10.txt").getLines().map(_.trim).toList
  val map = parseMap(input, identity, '.')
  println(s"pt1: ${pt1(map)}")
  println(s"pt2: ${pt2(map)}")