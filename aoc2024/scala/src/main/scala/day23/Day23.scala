package day23

import scala.annotation.tailrec
import scala.io.Source

type Graph = Map[String, List[String]]

def makePairs(nums: List[String]) =
  for {
    (x, idxX) <- nums.zipWithIndex
    (y, idxY) <- nums.zipWithIndex
    if idxX < idxY
  } yield List(x, y)

def pt1(graph: Map[String, List[String]]): Int =
  val testNodes = graph.keySet.filter(_.startsWith("t"))
  val components = findAll3Components(graph, testNodes)
  components.size

@tailrec
def findAll3Components(graph: Graph, testNodes: Set[String], result: Set[List[String]] = Set()): Set[List[String]] =
  if testNodes.isEmpty then
    result
  else
    val neighbours = graph(testNodes.head)
    val connectedNeighbours = makePairs(neighbours).filter(nodes => graph(nodes.head).contains(nodes.last))
    if connectedNeighbours.isEmpty then
      findAll3Components(graph, testNodes.tail, result)
    else
      val connectedComponents = connectedNeighbours.map(testNodes.head :: _).map(_.sorted)
      findAll3Components(graph, testNodes.tail, result ++ connectedComponents)


def pt2(graph: Map[String, List[String]]): String =
  val result = bronKerbosch(graph, Set(), graph.keySet, Set())
  val largestClique = result.maxBy(_.size)
  largestClique.toList.sorted.mkString(",")

def bronKerbosch(graph: Graph, r: Set[String], p: Set[String], x: Set[String], res: Set[Set[String]] = Set()): Set[Set[String]] =
  if p.isEmpty && x.isEmpty then
    res + r
  else
    var pUpdated = p
    var xUpdated = x
    val pivot = p.union(x).head
    p.diff(graph(pivot).toSet)
      .foldLeft(res)((acc, v) =>
        val neighbours = graph(v).toSet
        val result = bronKerbosch(graph, r + v, pUpdated.intersect(neighbours), xUpdated.intersect(neighbours), res)
        xUpdated = xUpdated + v
        pUpdated = pUpdated - v
        acc.union(result)
      )

def parseInput(lines: List[String]): Graph =
  lines.foldLeft(Map[String, List[String]]())((acc, line) =>
    val Array(l, r) = line.split("-")
    acc.updated(l, acc.getOrElse(l, Nil) :+ r)
      .updated(r, acc.getOrElse(r, Nil) :+ l)
  )

@main
def main(): Unit =
  val input = Source.fromResource("day23.txt").getLines().toList
  val graph = parseInput(input)
  println(s"pt1: ${pt1(graph)}")
  println(s"pt2: ${pt2(graph)}")