package day21

import util.{Map2D, Pos, parseMap}

import scala.annotation.tailrec
import scala.io.Source

case class State(start: Pos, path: List[Pos] = List())

extension (map: Map2D[Char])
  def neighbours(pos: Pos): List[Pos] =
    List(
      Pos(pos.x, pos.y - 1),
      Pos(pos.x + 1, pos.y),
      Pos(pos.x, pos.y + 1),
      Pos(pos.x - 1, pos.y)
    ).filterNot(map.isOutOfBounds)

def numpad = List("987", "654", "321", "#0A")

def getNumPadPaths: Map[(Char, Char), List[String]] =
  val map = parseMap(numpad, identity, ' ')
  val nodes = map.allKeys.filter(map(_) != '#').toList
  nodes.flatMap(start =>
    nodes.map(end =>
      (map(start), map(end)) -> numPadPaths(map, end, List(State(start)))
    )
  ).toMap

@tailrec
def numPadPaths(map: Map2D[Char], end: Pos, toVisit: List[State], result: List[List[Pos]] = List()): List[String] =
  if toVisit.isEmpty then
    result.map(_.map(map(_)).mkString(""))
  else if toVisit.head._1 == end then
    numPadPaths(map, end, toVisit.tail, toVisit.head._2.reverse :: result)
  else
    val next = toVisit.head
    val neighbours = map.neighbours(next._1)
      .filterNot(map(_) == '#')
      .filterNot(next._2.contains)
      .map(pos => State(pos, pos :: next._2))
    numPadPaths(map, end, toVisit.tail ++ neighbours, result)

def pathsBetween(c1: Char, c2: Char): List[String] = (c1, c2) match
  case ('A', 'v') => List("v<", "<v")
  case ('A', '<') => List("v<<", "<v<")
  case ('A', '^') => List("<")
  case ('A', '>') => List("v")
  case ('A', 'A') => List()

  case ('v', 'A') => List("^>", ">^")
  case ('v', '<') => List("<")
  case ('v', '^') => List("^")
  case ('v', '>') => List(">")
  case ('v', 'v') => List()

  case ('<', 'A') => List(">>^", ">^>")
  case ('<', 'v') => List(">")
  case ('<', '^') => List(">^")
  case ('<', '>') => List(">>")
  case ('<', '<') => List()

  case ('^', 'A') => List(">")
  case ('^', '<') => List("v<")
  case ('^', 'v') => List("v")
  case ('^', '>') => List("v>", ">v")
  case ('^', '^') => List()

  case ('>', 'A') => List("^")
  case ('>', '<') => List("<<")
  case ('>', '^') => List("^<", "<^")
  case ('>', 'v') => List("<")
  case ('>', '>') => List()

  case _ => throw new RuntimeException(s"Unhandled case: ($c1, $c2)")


  // Behöver bygga upp en map som ger kortaste vägen från en knapp till en annan
  // givet startinput, så kan man få ett antal möjliga vägar
  // varje sån väg bör man kunna skapa kortaste instruktionerna för med hjälp en map
  // och för varje instruktion i den behöver man göra samma sak
  // och sen en gång till
def pt1(line: String): Int =
  //val startPaths = List("<A^A^^>AvvvA", "<A^A>^^AvvvA", "<A^A^>^AvvvA") // 029A
  val startPaths = List("<")
  println(s"StartPath: ${startPaths.mkString("\n")}")
  val paths = startPaths.map(pressButtons(2))
  val length = paths.map(_.length)
  length.min

def pressButtons(level: Int)(path: String): List[String] =
  if level == 0 then
    List()
  else
    // Står på A till att börja med
    val instructions = path.foldLeft((List(), 'A'))((acc, c2) => (acc._1 :+ pathsBetween(acc._2, c2), c2))._1
    println(s"Instructions: $instructions")
    List()


@main
def main(): Unit =
  val input = Source.fromResource("day21.txt").getLines().toList
  val data = example.head
  println(s"pt1: ${pt1(data)}")
  println(s"pt2: ${}")

val example =
  """
    |029A
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList