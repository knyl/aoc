package day14

import day14.Direction.{EAST, NORTH, SOUTH, WEST}
import util.{Map2D, parseMap}

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

enum Direction:
  case NORTH, EAST, SOUTH, WEST

type Stones = Map2D[Char]
type State = Map[Map[(Int, Int), Char], Int]

def pt1(value: List[String]): Long =
  val map = parseMap(value, identity, '.')
  val stonesToRoll = map.map.filter(_._2 == 'O').keys.toList.sorted
  val updatedStones = stonesToRoll.foldLeft(map)((acc, stone) => rollStone(NORTH, stone, acc))
  calculateScore(updatedStones)

def pt2(value: List[String]): Long =
  val map = parseMap(value, identity, '.')
  val updatedStones = doCycles(1, 1000000000, map, Map() + (map.map -> 0))
  calculateScore(updatedStones)

@tailrec
def doCycles(cycle: Int, maxCycle: Int, stones: Stones, state: State): Stones =
  if cycle == maxCycle then stones
  else
    val newStones = oneCycle(stones)
    if state.contains(newStones.map) then
      val fromCycle = state(newStones.map)
      val iterationsLeft = (maxCycle - fromCycle) % (cycle - fromCycle)
      doCycles(0, iterationsLeft, newStones, Map())
    else
      doCycles(cycle + 1, maxCycle, newStones, state + (newStones.map -> cycle))

def oneCycle(stones: Stones): Stones =
  List(NORTH, WEST, SOUTH, EAST).foldLeft(stones)((acc, direction) => rollStones(acc, direction))

def rollStones(stones: Stones, direction: Direction): Stones =
  val stonesToRoll = stones.map.filter(_._2 == 'O').keys.toList.sortBy(sorting(direction))
  stonesToRoll.foldLeft(stones)((acc, stone) => rollStone(direction, stone, acc))

def sorting(direction: Direction)(stone: (Int, Int)): Int =
  direction match
    case Direction.NORTH => stone._2
    case Direction.EAST => -stone._1
    case Direction.SOUTH => -stone._2
    case Direction.WEST => stone._1

def calculateScore(stones: Stones): Long =
  val maxX = stones.map.keys.maxBy(_._1)._1
  stones.map.filter(_._2 == 'O').keys.toList.sorted.map(_._2).map(maxX - _ + 1).sum

def rollStone(direction: Direction, stone: (Int, Int), map: Stones): Stones =
  val newDestination = getDestination(stone, direction, map)
  if newDestination == stone then map
  else
    val updatedMap = map.map.updated(newDestination, 'O').updated(stone, '.')
    Map2D[Char](updatedMap, '.', map.width, map.height)

def nextPositionFun(direction: Direction) = direction match
  case Direction.NORTH => (x: Int, y: Int)
    => (x, y - 1)
  case Direction.EAST => (x: Int, y: Int)
    => (x + 1, y)
  case Direction.SOUTH => (x: Int, y: Int)
    => (x, y + 1)
  case Direction.WEST => (x: Int, y: Int)
    => (x - 1, y)

@tailrec
def getDestination(stone: (Int, Int), direction: Direction, map: Stones): (Int, Int) =
  val (x, y) = nextPositionFun(direction)(stone._1, stone._2)
  if isOutSideMap(x, y, map) || map(x, y) == '#' || map(x, y) == 'O' then stone
  else getDestination((x, y), direction, map)

def isOutSideMap(x: Int, y: Int, map: Stones): Boolean =
  x < 0 || y < 0 || x >= map.width || y >= map.height

@main
def main(): Unit =
  val lines = Source.fromResource("day14.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
