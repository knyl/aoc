package day12

import util.{Map2D, Pos, parseMap}

import scala.annotation.tailrec
import scala.io.Source

extension (map: Map2D[Char])
  def neighbours(pos: Pos): List[Pos] =
    List(
      Pos(pos.x, pos.y - 1),
      Pos(pos.x + 1, pos.y),
      Pos(pos.x, pos.y + 1),
      Pos(pos.x - 1, pos.y)
    )

def pt1(map: Map2D[Char]): Int =
  val regions = map.map.keys.map(p => getRegion(map, Set(p))).toSet
  val prices = regions.toList.map(r => price(map, r))
  prices.sum

def pt2(map: Map2D[Char]): Int =
  val regions = map.map.keys.map(p => getRegion(map, Set(p))).toSet
  val prices = regions.toList.map(r => price2(map, r))
  prices.sum

@tailrec
def getRegion(map: Map2D[Char], toVisit: Set[Pos], visited: Set[Pos] = Set()): Set[Pos] =
  if toVisit.isEmpty then visited
  else if visited.contains(toVisit.head) then getRegion(map, toVisit.tail, visited)
  else
    val next = toVisit.head
    val neighbours = map.neighbours(next)
      .filter(map(_) == map(next))
      .filterNot(map.isOutOfBounds)
      .filterNot(visited.contains)
    getRegion(map, toVisit.tail ++ neighbours, visited + next)

def price(map: Map2D[Char], region: Set[Pos]): Int =
  val perimeterCost = perimeter(map, region)
  val areaCost = region.size
  val price = perimeterCost * areaCost
  price

def price2(map: Map2D[Char], region: Set[Pos]): Int =
  val perimeterCost = perimeterDiscounted(map, region)
  val areaCost = region.size
  perimeterCost * areaCost

def perimeter(map: Map2D[Char], region: Set[Pos]): Int =
  region.toList
    .map(p => map.neighbours(p).filter(map(_) != map(region.head)))
    .map(_.length).sum

def perimeterDiscounted(map: Map2D[Char], region: Set[Pos]): Int =
  region.toList.map(p => getCorners(map, p)).sum

def getCorners(map: Map2D[Char], position: Pos): Int = matchingNeighbours(map, position) match
  case Nil => 4
  case _ :: Nil => 2
  case n1 :: n2 :: Nil if n1.x == n2.x || n1.y == n2.y => 0
  case n1 :: n2 :: Nil => 2 - getInnerCorner(map, position, n1, n2)
  case n1 :: n2 :: n3 :: Nil => 2 - getInnerCorner(map, position, n1, n2, n3)
  case neighbours if neighbours.length == 4 => 4 - getAllInnerCorner(map, position)
  case _ => throw new RuntimeException("Invalid number of neighbours")

def getInnerCorner(map: Map2D[Char], pos: Pos, n1: Pos, n2: Pos): Int =
  val corner =
    if n1.x == pos.x then Pos(n2.x, n1.y)
    else Pos(n1.x, n2.y)
  if map(corner) != map(pos) then 0 else 1

def getInnerCorner(map: Map2D[Char], pos: Pos, n1: Pos, n2: Pos, n3: Pos): Int =
  val corners =
    if n1.x == n2.x then List(Pos(n3.x, n1.y), Pos(n3.x, n2.y))
    else if n1.x == n3.x then List(Pos(n2.x, n1.y), Pos(n2.x, n3.y))
    else if n2.x == n3.x then List(Pos(n1.x, n2.y), Pos(n1.x, n3.y))
    else if n1.y == n2.y then List(Pos(n1.x, n3.y), Pos(n2.x, n3.y))
    else if n1.y == n3.y then List(Pos(n1.x, n2.y), Pos(n3.x, n2.y))
    else if n2.y == n3.y then List(Pos(n2.x, n1.y), Pos(n3.x, n1.y))
    else throw new RuntimeException("Invalid inner corner")
  corners.count(map(_) == map(pos))

def getAllInnerCorner(map: Map2D[Char], pos: Pos): Int =
  List(Pos(pos.x + 1, pos.y + 1), Pos(pos.x - 1, pos.y - 1), Pos(pos.x + 1, pos.y - 1), Pos(pos.x - 1, pos.y + 1))
    .count(map(_) == map(pos))

def matchingNeighbours(map: Map2D[Char], pos: Pos): List[Pos] =
  map.neighbours(pos).filterNot(map.isOutOfBounds).filter(map(_) == map(pos))

@main
def main(): Unit =
  val input = Source.fromResource("day12.txt").getLines().map(_.trim).toList
  val map = parseMap(input, identity, '.')
  println(s"pt1: ${pt1(map)}")
  println(s"pt2: ${pt2(map)}")
