package day25

import scala.io.Source

def pt1(locks: List[List[Int]], keys: List[List[Int]]): Int =
  locks.map(lock =>
    keys.count(key => open(lock, key))
  ).sum

def open(lock: List[Int], key: List[Int]): Boolean =
  lock.zip(key).forall((l, k) => l + k <= 5)

def parseObject(str: String) =
  str
    .split("\n")
    .tail
    .dropRight(1)
    .toList
    .transpose
    .map(l => l.count(_ == '#'))

def parseInput(input: List[String]): (List[List[Int]], List[List[Int]]) =
  val locks = input
    .filter(s => s.startsWith("#####") && s.endsWith("....."))
    .map(parseObject)
  val keys = input
    .filter(s => s.startsWith(".....") && s.endsWith("#####"))
    .map(parseObject)
  (locks, keys)

@main
def main(): Unit =
  val input = Source.fromResource("day25.txt").mkString.split("\n\n").map(_.trim).toList
  val (locks, keys) = parseInput(input)
  println(s"pt1: ${pt1(locks, keys)}")
