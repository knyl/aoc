package day01
import scala.io.Source

@main
def main(): Unit =
  val lines = Source.fromResource("day01.txt").getLines().toList

  println("Pt1: " + lines.length)
