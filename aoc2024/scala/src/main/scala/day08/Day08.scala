package day08

import util.{Map2D, Pos, parseMap, printMap}

import scala.io.Source

def makePairs(nums: List[Pos]) =
  for {
    (x, idxX) <- nums.zipWithIndex
    (y, idxY) <- nums.zipWithIndex
    if idxX < idxY
  } yield (x, y)

def pt1(map: Map2D[Char]): Int =
  val antennas = getAntennas(map)
  antennas.flatMap(getAntinodes(map, _)).size

def getAntinodes(value: Map2D[Char], c: Char): List[Pos] =
  val antennas = value.map.filter(_._2 == c).keys.toSet
  val antennaPairs = makePairs(antennas.toList)
  antennaPairs.flatMap((a1, a2) =>
    val (distX, distY) = getDistance(a1, a2)
    if a1.x > a2.x && a1.y > a2.y then
      List(Pos(a1.x + distX, a1.y + distY), Pos(a2.x - distX, a2.y - distY)).filterNot(value.isOutOfBounds)
    else if a1.x < a2.x && a1.y < a2.y then
      List(Pos(a1.x - distX, a1.y - distY), Pos(a2.x + distX, a2.y + distY)).filterNot(value.isOutOfBounds)
    else if a1.y == a2.y then
      List(Pos(a1.x + distX, a1.y), Pos(a2.x - distX, a2.y)).filterNot(value.isOutOfBounds)
    else if a1.x > a2.x && a1.y < a2.y then
      List(Pos(a1.x + distX, a1.y - distY), Pos(a2.x - distX, a2.y + distY)).filterNot(value.isOutOfBounds)
    else
      List(Pos(a1.x - distX, a1.y + distY), Pos(a2.x + distX, a2.y - distY)).filterNot(value.isOutOfBounds)
  )

def getDistance(p1: Pos, p2: Pos): (Int, Int) = (math.abs(p1.x - p2.x), math.abs(p1.y - p2.y))

def getAntennas(map: Map2D[Char]): Set[Char] =
  map.map.values.toSet

def pt2(map: Map2D[Char]): Int =
  val antennas = getAntennas(map)
  val antinodes = antennas.flatMap(getAntinodes2(map, _))
  antinodes.union(map.map.keySet).size

def getAntinodes2(map: Map2D[Char], c: Char): List[Pos] =
  val antennas = map.map.filter(_._2 == c).keys.toSet
  val antennaPairs = makePairs(antennas.toList)
  val maxLen = Math.max(map.width, map.height)
  val range = 1 to maxLen
  antennaPairs.flatMap((a1, a2) =>
    val (distX, distY) = getDistance(a1, a2)

    if a1.x > a2.x && a1.y > a2.y then
      range.flatMap(n => List(Pos(a1.x + distX * n, a1.y + distY * n), Pos(a2.x - distX * n, a2.y - distY * n)))
        .filterNot(map.isOutOfBounds)
    else if a1.x < a2.x && a1.y < a2.y then
      range.flatMap(n => List(Pos(a1.x - distX * n, a1.y - distY * n), Pos(a2.x + distX * n, a2.y + distY * n)))
        .filterNot(map.isOutOfBounds)
    else if a1.y == a2.y then
      range.flatMap(n => List(Pos(a1.x + distX * n, a1.y), Pos(a2.x - distX * n, a2.y)))
        .filterNot(map.isOutOfBounds)
    else if a1.x > a2.x && a1.y < a2.y then
      range.flatMap(n => List(Pos(a1.x + distX * n, a1.y - distY * n), Pos(a2.x - distX * n, a2.y + distY * n)))
        .filterNot(map.isOutOfBounds)
    else
      range.flatMap(n => List(Pos(a1.x - distX * n, a1.y + distY * n), Pos(a2.x + distX * n, a2.y - distY * n)))
        .filterNot(map.isOutOfBounds)
  )

@main
def main(): Unit =
  val input = Source.fromResource("day08.txt").getLines().map(_.trim).toList
  val map = parseMap(input, identity, '.')
  println(s"pt1: ${pt1(map)}")
  println(s"pt2: ${pt2(map)}")
