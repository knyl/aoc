package day22

import scala.annotation.tailrec
import scala.io.Source

def pt1(numbers: List[Long]): BigInt =
  numbers.map(getSecretNumber(_, 2000)).map(BigInt(_)).sum

def mix(number: Long, secretNumber: Long) = secretNumber ^ number
def prune(secretNumber: Long) = secretNumber % 16777216

@tailrec
def getSecretNumber(number: Long, rounds: Int): Long =
  if rounds == 0 then number
  else
    var secretNumber = prune(mix(number * 64, number))
    secretNumber = prune(mix(secretNumber / 32, secretNumber))
    secretNumber = prune(mix(secretNumber * 2048, secretNumber))
    getSecretNumber(secretNumber, rounds - 1)

@main
def main(): Unit =
  val input = Source.fromResource("day22.txt").getLines().toList
  val data = input.map(_.toLong)
  println(s"pt1: ${pt1(data)}")
  println(s"pt2: ${}")

val example =
  """
    |1
    |10
    |100
    |2024
    |""".stripMargin.split("\n").map(_.trim).filterNot(_.isBlank).toList