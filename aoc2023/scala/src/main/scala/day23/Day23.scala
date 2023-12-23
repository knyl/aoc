package day23

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps

type Map2D = Map[Pos, Char]

case class Pos(x: Int, y: Int)

def pt1(lines: List[String]): Int =
  val (map, width, height) = parseMap(lines)
  val isEnd = (p: Pos) => p.x == width - 2 && p.y == height - 1
  val slopes = findSlopes(Set(Pos(1, 0)), Set.empty, map) + Pos(1, 0)
  val nodes = slopes.map(s => (s, getPaths(s, map, isEnd))).toMap
  val path = dfsAllPaths(Pos(1, 0), nodes, isEnd)
  path.map(_._2).sum

def pt2(lines: List[String]): Int =
  val (map, width, height) = parseMap(lines)
  val isEnd = (p: Pos) => p.x == width - 2 && p.y == height - 1
  val slopes = findSlopes(Set(Pos(1, 0)), Set.empty, map) + Pos(1, 0)
  val nodes = slopes.map(s => (s, getPaths2(s, map, isEnd))).toMap
  val path = dfsAllPaths(Pos(1, 0), nodes, isEnd)
  path.map(_._2).sum

def getPaths(slope: Pos, map: Map2D, isEnd: Pos => Boolean): List[(Pos, Int)] =
  val toVisit = neighbours(slope, map)
  toVisit.map(p => getPath(Set(p), Set(slope), map, isEnd)).toList

@tailrec
def getPath(toVisit: Set[Pos], visited: Set[Pos], map: Map2D, isEnd: Pos => Boolean): (Pos, Int) = toVisit.headOption match
  case None => throw new Exception("No path found")
  case Some(pos) if isEnd(pos) || surroundedBySlopes(pos, map) => (pos, visited.size)
  case Some(pos) =>
    val newToVisit = neighbours(pos, map).diff(visited)
    getPath(toVisit.tail ++ newToVisit, visited + pos, map, isEnd)

def getPaths2(slope: Pos, map: Map2D, isEnd: Pos => Boolean): List[(Pos, Int)] =
  val toVisit = allNeighbours(slope, map)
  toVisit.map(p => getPath2(Set(p), Set(slope), map, isEnd)).toList

@tailrec
def getPath2(toVisit: Set[Pos], visited: Set[Pos], map: Map2D, isEnd: Pos => Boolean): (Pos, Int) = toVisit.headOption match
  case None =>
    println(visited.toList.sortBy(p => (p.y, p.x)))
    throw new Exception("No path2 found")
  case Some(pos) if isStart(pos) || isEnd(pos) || surroundedBySlopes(pos, map) => (pos, visited.size)
  case Some(pos) =>
    val newToVisit = allNeighbours(pos, map).diff(visited)
    getPath2(toVisit.tail ++ newToVisit, visited + pos, map, isEnd)

def isStart(pos: Pos): Boolean = pos.x == 1 && pos.y == 0

@tailrec
def findSlopes(toVisit: Set[Pos], visited: Set[Pos], map: Map2D, visitedSlopes: Set[Pos] = Set()): Set[Pos] = toVisit.headOption match
  case None => visitedSlopes
  case Some(pos) =>
    val newToVisit = neighbours(pos, map).diff(visited)
    val slope = surroundedBySlopes(pos, map)
    if slope then
      findSlopes(toVisit.tail ++ newToVisit, visited + pos, map, visitedSlopes + pos)
    else findSlopes(toVisit.tail ++ newToVisit, visited + pos, map, visitedSlopes)

def allNeighbours(p: Pos, map: Map2D): Set[Pos] =
  Set(Pos(p.x, p.y - 1), Pos(p.x, p.y + 1), Pos(p.x - 1, p.y), Pos(p.x + 1, p.y))
    .filter(pos => map.getOrElse(pos, '#') != '#')

def surroundedBySlopes(pos: Pos, map: Map2D): Boolean =
  allNeighbours(pos, map).forall(posIsSlope(_, map))

def posIsSlope(pos: Pos, d: Map2D): Boolean =
  val neighbour = d.getOrElse(pos, '#')
  neighbour == '#' || neighbour == '<' || neighbour == '>' || neighbour == '^' || neighbour == 'v'

def neighbours(p: Pos, map: Map2D): Set[Pos] =
  val up = Pos(p.x, p.y - 1)
  val down = Pos(p.x, p.y + 1)
  val left = Pos(p.x - 1, p.y)
  val right = Pos(p.x + 1, p.y)
  List(
    if map.get(up).exists(p => p == '.' || p == '^') then Some(up) else None,
    if map.get(down).exists(p => p == '.' || p == 'v') then Some(down) else None,
    if map.get(left).exists(p => p == '.' || p == '<') then Some(left) else None,
    if map.get(right).exists(p => p == '.' || p == '>') then Some(right) else None).flatten.toSet

def dfsAllPaths(node: Pos,
                adjacent: Map[Pos, List[(Pos, Int)]],
                isEnd: Pos => Boolean,
                dp: Map[Pos, Int] = Map(),
                visited: Set[Pos] = Set(),
                path: List[(Pos, Int)] = List()): List[(Pos, Int)] =
  if isEnd(node) then path
  else
    val newVisited = visited + node
    val children = adjacent.getOrElse(node, List.empty)
    val notVisitedChildren = children.filter((p, _) => !visited.contains(p))
    val newPaths = notVisitedChildren.map((p, len) => dfsAllPaths(p, adjacent, isEnd, dp, newVisited, path :+ (p, len)))
    if newPaths.isEmpty then path
    else newPaths.maxBy(_.map(_._2).sum)

def parseMap(lines: List[String]): (Map2D, Int, Int) =
  val width = lines.headOption.map(_.length).getOrElse(0)
  val height = lines.length
  val map = lines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.map { (c, x) =>
      Pos(x, y) -> c
    }
  }.toMap
  (map, width, height)

@main
def main(): Unit =
  val lines = Source.fromResource("day23.txt").getLines().map(_.trim).toList

  println("Pt1: " + pt1(lines))
  println("Pt2: " + pt2(lines))
