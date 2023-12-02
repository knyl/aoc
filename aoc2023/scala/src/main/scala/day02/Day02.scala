package day02

import scala.io.Source

case class Game(id: Int, cubes: List[Cubes])

case class Cubes(red: Int, green: Int, blue: Int)

case class Bag(red: Int, green: Int, blue: Int)

def gamePt1(line: String): Int =
  val bag = Bag(12, 13, 14)
  val game = parseGame(line)
  val allPossible = game.cubes.forall(c => isPossible(c, bag))
  if allPossible then game.id else 0

def isPossible(cubes: Cubes, bag: Bag): Boolean =
  val redPossible = cubes.red <= bag.red
  val greenPossible = cubes.green <= bag.green
  val bluePossible = cubes.blue <= bag.blue
  redPossible && greenPossible && bluePossible

def gamePt2(line: String): Int =
  val game = parseGame(line)
  val fewestColors = game.cubes.reduce((c1, c2) => getFewestColors(c1, c2))
  fewestColors.red * fewestColors.green * fewestColors.blue

def getFewestColors(c1: Cubes, c2: Cubes): Cubes =
  val red = Math.max(c1.red, c2.red)
  val green = Math.max(c1.green, c2.green)
  val blue = Math.max(c1.blue, c2.blue)
  Cubes(red, green, blue)

def parseGame(line: String): Game =
  line match
    case s"Game $id: $cubes" =>
      val cubesList = cubes.split(";").toList.map(parseCubes)
      Game(id.toInt, cubesList)

def parseCubes(cubes: String): Cubes =
  val red = parseColor(cubes, "red")
  val green = parseColor(cubes, "green")
  val blue = parseColor(cubes, "blue")
  Cubes(red, green, blue)

def parseColor(cubes: String, color: String): Int =
  val regex = s".*?([0-9]+) $color.*".r
  cubes match
    case regex(number) => number.toInt
    case _ => 0

@main
def main(): Unit =
  val lines = Source.fromResource("day02.txt").getLines().toList

  println("Pt1: " + lines.map(gamePt1).sum)
  println("Pt2: " + lines.map(gamePt2).sum)

val example =
  """
    |Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
    |""".stripMargin.split("\n").toList.filter(s => !s.isBlank)