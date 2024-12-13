package day13

import scala.io.Source

case class Game(a: Button, b: Button, prize: Prize)
case class Button(x: BigInt, y: BigInt)
case class Prize(x: BigInt, y: BigInt)

def pt1(data: List[Game]): BigInt =
  data.map(getCost).sum

def getCost(game: Game): Int =
  var cost: Option[Int] = None
  for i <- 0 to 100 do
    for j <- 0 to 100 do
      val scoreX = i * game.a.x + j * game.b.x
      val scoreY = i * game.a.y + j * game.b.y
      if scoreX == game.prize.x && scoreY == game.prize.y then
        cost = Some(3 * i + j)
  cost.getOrElse(0)

def pt2(data: List[Game]): BigInt =
  val cost = BigInt("10000000000000")
  data.map(g => getCost2(Game(g.a, g.b, Prize(g.prize.x + cost, g.prize.y + cost)))).sum

def getCost2(g: Game): BigInt =
  if noSolution(g) then 0
  else
    val detX = g.prize.x * g.b.y - g.prize.y * g.b.x
    val detY = g.a.x * g.prize.y - g.a.y * g.prize.x
    val det = g.a.x * g.b.y - g.a.y * g.b.x

    val x = BigDecimal(detX) / BigDecimal(det)
    val y = BigDecimal(detY) / BigDecimal(det)
    if x.toBigIntExact.isDefined && y.toBigIntExact.isDefined then
      3 * x.toBigInt + y.toBigInt
    else BigInt(0)

def noSolution(game: Game): Boolean =
  val det = game.a.x * game.b.y - game.a.y * game.b.x
  det == 0

def parseInput(input: List[String]): List[Game] =
  input.map(_.split("\n").map(_.trim).toList).map {
    case List(a, b, p) => Game(parseButton(a), parseButton(b), parsePrize(p))
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
  println(s"pt2: ${pt2(data)}")
