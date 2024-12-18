package day18

import util.{Pos, printMap}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

extension (pos: Pos)
  def neighbours: List[Pos] =
    List(
      Pos(pos.x, pos.y - 1),
      Pos(pos.x + 1, pos.y),
      Pos(pos.x, pos.y + 1),
      Pos(pos.x - 1, pos.y)
    )

def pt1(data: Set[Pos]): Int =
  val xMax = 70
  val yMax = 70
  val from = Pos(0, 0)
  val to = Pos(xMax, yMax)
  val queue = mutable.PriorityQueue[(Pos, Int)]((from, 0))(Ordering.by(-_._2))
  search(queue, to, data)

@tailrec
def search(toVisit: mutable.PriorityQueue[(Pos, Int)], to: Pos, data: Set[Pos], visited: Set[Pos] = Set()): Int =
  if toVisit.isEmpty then 
    -1 
  else 
    val next = toVisit.dequeue()
    if next._1 == to then 
      next._2
    else if visited.contains(next._1) then 
      search(toVisit, to, data, visited)
    else
      val neighbours = next._1.neighbours
        .filterNot(visited.contains)
        .filterNot(data.contains)
        .filterNot(isOutOfBounds)
        .map((_, next._2 + 1))
      neighbours.foreach(n => toVisit.enqueue(n))
      search(toVisit, to, data, visited + next._1)

def isOutOfBounds(pos: Pos): Boolean =
  val xMax = 70
  val yMax =  70
  pos.x < 0 || pos.y < 0 || pos.x > xMax || pos.y > yMax

def pt2(data: List[Pos]): String =
  val i = (1025 until data.length).find(i => pt1(data.take(i).toSet) == -1).get
  s"${data(i - 1).x},${data(i - 1).y}"

@main
def main(): Unit =
  val input = Source.fromResource("day18.txt").getLines().map(_.trim).toList
  val data = input.map(_.split(",").map(_.toInt)).map(a => Pos(a(0), a(1)))
  println(s"pt1: ${pt1(data.take(1024).toSet)}")
  println(s"pt2: ${pt2(data)}")
