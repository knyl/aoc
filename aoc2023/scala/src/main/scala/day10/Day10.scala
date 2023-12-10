package day10

import util.{Map2D, Pos, parseMap}

import scala.annotation.tailrec
import scala.io.Source

type Tiles = Map2D[Char]

def pt1(value: List[String]): Int =
  val tiles = parseMap(value, identity, '.')
  val start = tiles.map.find(_._2 == 'S').get._1
  val startNeighbors = neighbors(start, tiles)
  visit(startNeighbors.map((_, 1)), Map((start, 0)), tiles).values.max

@tailrec
def visit(toVisit: List[(Pos, Int)], visited: Map[Pos, Int], tiles: Tiles): Map[Pos, Int] =
  toVisit.headOption match
    case None => visited
    case Some(pos, distance) =>
      val newDistances = getNeighbors(pos, tiles).filterNot(visited.contains).map(_ -> (distance + 1)).toMap
      val newVisited = visited ++ List(toVisit.head)
      visit(toVisit.tail ++ newDistances, newVisited, tiles)

def getNeighbors(pos: Pos, tiles: Tiles): List[Pos] =
  val (x, y) = pos
  tiles(pos) match
    case 'J' => List((x, y - 1), (x - 1, y))
    case 'L' => List((x, y - 1), (x + 1, y))
    case 'F' => List((x, y + 1), (x + 1, y))
    case '7' => List((x, y + 1), (x - 1, y))
    case '|' => List((x, y - 1), (x, y + 1))
    case '-' => List((x - 1, y), (x + 1, y))
    case tile => throw new Exception(s"Unknown tile: $pos, $tile")

def neighbors(pos: Pos, tiles: Tiles): List[Pos] =
  val (x, y) = pos
  List((x, y - 1), (x - 1, y), (x + 1, y), (x, y + 1)).filter(p => isConnected(p, pos, tiles))

def isConnected(pos: Pos, start: Pos, tiles: Tiles): Boolean =
  val (x, y) = pos
  tiles(pos) match
    case 'J' => List((x, y - 1), (x - 1, y)).contains(start)
    case 'L' => List((x, y - 1), (x + 1, y)).contains(start)
    case 'F' => List((x, y + 1), (x + 1, y)).contains(start)
    case '7' => List((x, y + 1), (x - 1, y)).contains(start)
    case '|' => List((x, y - 1), (x, y + 1)).contains(start)
    case '-' => List((x - 1, y), (x + 1, y)).contains(start)
    case 'S' => false
    case '.' => false
    case tile => throw new Exception(s"Unknown tile: $pos, $tile")

def pt2(value: List[String]): Int =
  val tiles = parseMap(value, identity, '.')
  val start = tiles.map.find(_._2 == 'S').get._1
  val startNeighbors = neighbors(start, tiles)
  val loopNodes = visit(startNeighbors.map((_, 1)), Map((start, 0)), tiles).keys.toSet
  val nodesToCheck = tiles.map.keys.filter(!loopNodes.contains(_)).toList
  val nodesInside = nodesToCheck.map(countOverlaps(_, loopNodes, tiles)).filter(_._2 % 2 != 0)
  nodesInside.length

def countOverlaps(node: Pos, loopNodes: Set[Pos], tiles: Tiles): (Pos, Int) =
  val nodesToCheck = loopNodes.filter(n => n._1 == node._1 && n._2 > node._2)
  val straightOverlaps = nodesToCheck.filter(p => crossingPipe(p, tiles))
  val cornerOverlaps = checkCornerCrossings(nodesToCheck.toList.sorted, tiles)
  (node, straightOverlaps.size + cornerOverlaps)

@tailrec
def checkCornerCrossings(nodes: List[Pos], tiles: Tiles, prevCorner: Option[Char] = None, acc: Int = 0): Int = nodes.headOption match
  case None => acc
  case Some(pos) if prevCorner.isDefined =>
    (prevCorner.get, tiles(pos)) match
      case ('7', 'L') | ('F', 'J') => checkCornerCrossings(nodes.tail, tiles, None, acc + 1)
      case ('F', 'L') | ('7', 'J') => checkCornerCrossings(nodes.tail, tiles, None, acc)
      case _ => checkCornerCrossings(nodes.tail, tiles, prevCorner, acc)
  case Some(pos) =>
    tiles(pos) match
      case tile if tile == '7' || tile == 'F' => checkCornerCrossings(nodes.tail, tiles, Some(tile), acc)
      case _ => checkCornerCrossings(nodes.tail, tiles, prevCorner, acc)

def crossingPipe(pos: Pos, tiles: Tiles): Boolean = tiles(pos) == '-'

@main
def main(): Unit =
  val lines = Source.fromResource("day10.txt").getLines().filter(!_.isBlank).map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
