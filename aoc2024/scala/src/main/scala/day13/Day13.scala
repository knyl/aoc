package day13

import scala.annotation.tailrec
import scala.io.Source
import scala.util.boundary

case class Game(a: Button, b: Button, prize: Prize)
case class Button(x: Int, y: Int)
case class Prize(x: Int, y: Int)

def pt1(data: List[Game]): Int =
  data.map(getCost(_)).sum

@tailrec
def getCost(game: Game, i: Int = 0, j: Int = 0): Int =
  val scoreX = i * game.a.x + j * game.b.x
  val scoreY = i * game.a.y + j * game.b.y
  if scoreX == game.prize.x && scoreY == game.prize.y then 3 * i + j
  else if i > 100 && j > 100 then 0
  else if scoreX > game.prize.x || scoreY > game.prize.y then getCost(game, i + 1)
  else if j > 100 then getCost(game, i + 1)
  else getCost(game, i, j + 1)


def parseInput(input: List[String]): List[Game] =
  input.map(_.split("\n").map(_.trim).toList).map {
    case List(a, b, p) =>
      Game(parseButton(a), parseButton(b), parsePrize(p))
    case _ => throw new IllegalArgumentException("Invalid input")
  }

def parseButton(str: String): Button = str match
  case s"Button $b: X+${x}, Y+${y}" => Button(x.toInt, y.toInt)
  case _ => throw new IllegalArgumentException(s"Invalid input: $str")

def parsePrize(str: String): Prize = str match
  case s"Prize: X=${x}, Y=${y}" => Prize(x.toInt, y.toInt)
  case _ => throw new IllegalArgumentException(s"Invalid input: $str")

@main
def main(): Unit =
  val input = Source.fromResource("day13.txt").mkString.split("\n\n").map(_.trim).toList
  val data = parseInput(input)
  println(s"pt1: ${pt1(data)}")
  println(s"pt2: ${}")


val example =
  """
    |Button A: X+94, Y+34
    |Button B: X+22, Y+67
    |Prize: X=8400, Y=5400
    |
    |Button A: X+26, Y+66
    |Button B: X+67, Y+21
    |Prize: X=12748, Y=12176
    |
    |Button A: X+17, Y+86
    |Button B: X+84, Y+37
    |Prize: X=7870, Y=6450
    |
    |Button A: X+69, Y+23
    |Button B: X+27, Y+71
    |Prize: X=18641, Y=10279
    |""".stripMargin.split("\n\n").map(_.trim).filterNot(_.isBlank).toList