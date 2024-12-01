package day17

import scala.io.Source

case class Position(x: Int, y: Int)
case class Piece(value: List[Position])

@main
def main(): Unit =
  val input = Source.fromResource("day17.txt").getLines().toList

val TEST = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

val PIECES = List(
  Piece(List(Position(0, 0), Position(1, 0), Position(2, 0), Position(3, 0))),
  Piece(List(Position(1, 0), Position(0, 1), Position(1, 1), Position(2, 1), Position(2, 1))),
  Piece(List(Position(0, 2), Position(1, 2), Position(2, 0), Position(2, 1), Position(2, 2))),
  Piece(List(Position(0, 0), Position(0, 1), Position(0, 2), Position(0, 3))),
  Piece(List(Position(0, 0), Position(0, 1), Position(1, 0), Position(1, 1)))
)