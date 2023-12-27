package day25

import scala.annotation.tailrec
import scala.io.Source
import scala.language.postfixOps
import scala.util.Random

type Graph = Map[String, List[String]]

def pt1(lines: List[String]): Int =
  val rand = new Random()
  val graph = parseInput(lines)
  val result = karger(graph, graph, rand)
  val comp1 = result.head._1
  val comp2 = result.last._1
  comp1.split("-").length * comp2.split("-").length

@tailrec
def karger(graph: Graph, originalGraph: Graph, rand: Random): Graph =
  if graph.size == 2 && graph.head._2.size == 3 then graph
  else if graph.size == 2 then karger(originalGraph, originalGraph, rand)
  else
    val nodeFromInd = rand.nextInt(graph.size)
    val nodeFrom = graph.keys.toList(nodeFromInd)
    val nodeToInd = rand.nextInt(graph(nodeFrom).size)
    val nodeTo = graph(nodeFrom)(nodeToInd)
    val newGraph = mergeNodes(graph, nodeFrom, nodeTo).filter((_, v) => v.nonEmpty)
    karger(newGraph, originalGraph, rand)

def mergeNodes(graph: Graph, nodeFrom: String, nodeTo: String): Graph =
  val newNode = s"$nodeFrom-$nodeTo"
  val node1Neighbours = graph(nodeFrom).filter(_ != nodeTo)
  val node2Neighbours = graph(nodeTo).filter(_ != nodeFrom)
  val newNeighbours = node1Neighbours ++ node2Neighbours
  val updatedNeighbours = updateNeighbours(graph, newNode, nodeFrom, nodeTo)
  updatedNeighbours.removed(nodeFrom).removed(nodeTo) + (newNode -> newNeighbours)

def updateNeighbours(graph: Graph, newNode: String, nodeFrom: String, nodeTo: String): Graph =
  graph.map((k, v) => k -> v.map(n => if n == nodeFrom || n == nodeTo then newNode else n))

def parseInput(lines: List[String]): Graph =
  lines.flatMap(parseLine).groupBy(_._1).map((k, v) => k -> v.map(_._2))

def parseLine(line: String): List[(String, String)] = line match
  case s"$from: $to" => createEdges(from, to.trim.split(" ").toList)
  case _ => throw new RuntimeException(s"Invalid line: $line")

def createEdges(from: String, neighbours: List[String]): List[(String, String)] =
  neighbours.flatMap(n => List((from, n), (n, from)))

@main
def main(): Unit =
  val lines = Source.fromResource("day25.txt").getLines().map(_.trim).toList

  println("Pt1: " + pt1(lines))

val example =
  """
    |jqt: rhn xhk nvd
    |rsh: frs pzl lsr
    |xhk: hfx
    |cmg: qnr nvd lhk bvb
    |rhn: xhk bvb hfx
    |bvb: xhk hfx
    |pzl: lsr hfx nvd
    |qnr: nvd
    |ntq: jqt hfx bvb xhk
    |nvd: lhk
    |lsr: lhk
    |rzs: qnr cmg lsr rsh
    |frs: qnr lhk lsr
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList
