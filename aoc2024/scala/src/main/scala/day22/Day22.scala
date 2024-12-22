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

def pt2(numbers: List[Long]): BigInt =
  val secretNumbers = numbers.map(getPrices(_, 2001))
  val secretNumbersWithDiff = secretNumbers.map(calculateDiffs)
  getMaxSum(secretNumbersWithDiff)

def getMaxSum(numbers: List[List[(Long, Long)]]): Long =
  val allDiffSequences = numbers.flatMap(_.map(_._2).sliding(4).map(toTuple)).toSet
  var maxSum = Long.MinValue
  var counter = 0
  val processedNumbers = numbers.map(ns =>
    ns
      .sliding(4)
      .map(lst => (lst.map(_._2), lst.last._1))
      .filter(_._2 > 0)
      .toList
      .reverse
      .map(p => toTuple(p._1) -> p._2)
      .toMap
  )
  for (diffSequence <- allDiffSequences)
    var currentSum = 0L
    var searching = true
    var currentNumbers = processedNumbers
    if (counter % 100 == 0) println(s"counter: ${counter}")
    counter += 1
    while (searching)
      if (currentNumbers.length * 9 + currentSum < maxSum) || currentNumbers.isEmpty then
        searching = false
      else
        val next = currentNumbers.head
        val nextNum = next.getOrElse(diffSequence, 0L)
        currentSum += nextNum
        currentNumbers = currentNumbers.tail

    if currentSum > maxSum then
      maxSum = currentSum
  maxSum

def toTuple(list: List[Long]): (Long, Long, Long, Long) =
  (list.head, list(1), list(2), list(3))

def calculateDiffs(secretNumbers: List[Long]): List[(Long, Long)] =
  secretNumbers.zip(secretNumbers.tail).map((a, b) => (b, b - a))

@tailrec
def getPrices(number: Long, rounds: Int, result: List[Long] = List()): List[Long] =
  if rounds == 1 then (result :+ number).map(_ % 10)
  else
    var secretNumber = prune(mix(number * 64, number))
    secretNumber = prune(mix(secretNumber / 32, secretNumber))
    secretNumber = prune(mix(secretNumber * 2048, secretNumber))
    getPrices(secretNumber, rounds - 1, result :+ number)


@main
def main(): Unit =
  val input = Source.fromResource("day22.txt").getLines().toList
  val data = input.map(_.toLong)
  println(s"pt1: ${pt1(data)}")
  println(s"pt2: ${pt2(data)}")