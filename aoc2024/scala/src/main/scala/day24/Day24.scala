package day24

import java.math.BigInteger
import scala.io.Source

trait Gate:
  val regNameL: String
  val regNameR: String
  val regNameOut: String
  def output(v1: Int, v2: Int): Int

case class Register(name: String, value: Int)
case class And(regNameL: String, regNameR: String, regNameOut: String) extends Gate:
  override def output(v1: Int, v2: Int): Int = v1 & v2
case class Or(regNameL: String, regNameR: String, regNameOut: String) extends Gate:
  override def output(v1: Int, v2: Int): Int = v1 | v2
case class Xor(regNameL: String, regNameR: String, regNameOut: String) extends Gate:
  override def output(v1: Int, v2: Int): Int = v1 ^ v2


def pt1(inputs: List[Register], gates: List[Gate]): BigInt =
  val outputs = gates.map(_.regNameOut).filter(_.startsWith("z")).sorted
  val outputMap = gates.map(g => g.regNameOut -> g).toMap
  val resultMap: Map[String, Int] = inputs.map(r => r.name -> r.value).toMap
  val finalResultMap = outputs.foldLeft(resultMap)((acc, output) => calculateOutput(outputMap, acc, output))
  val resultString = outputs.reverse.map(finalResultMap).mkString("")
  BigInt(resultString, 2)

def calculateOutput(gateMap: Map[String, Gate], resultMap: Map[String, Int], output: String): Map[String, Int] =
  if resultMap.contains(output) then resultMap
  else
    val gate = gateMap(output)
    val updatedResultMap = calculateOutput(gateMap, resultMap, gate.regNameL)
    val finalResultMap = calculateOutput(gateMap, updatedResultMap, gate.regNameR)
    finalResultMap.updated(output, gate.output(finalResultMap(gate.regNameL), finalResultMap(gate.regNameR)))

def pt2(gates: List[Gate]): Unit =
  println("----")
  (0 to 44)
    .map(String.format("%02d", _))
    .foreach(printAdder(gates, _))
  //printAdder(gates, "11") // z11, wpd
  //printAdder(gates, "15") // skh, jqf
  //printAdder(gates, "19") // z19, mdd
  //printAdder(gates, "37") // z37, wts
  val res = List("z11", "wpd", "skh", "jqf", "z19", "mdd", "z37", "wts").sorted.mkString(",")
  println(res)

def printAdder(gates: List[Gate], level: String): Unit =
  val lvl1a = getGate(gates, s"x$level")
  val lvl1b = getGate(gates, s"y$level")
  val lvl2a = lvl1a.union(lvl1b).flatMap(g => getGate(gates, g.regNameOut))
  lvl1b.union(lvl1a).toList.sortBy(_.getClass.getSimpleName).foreach(println)
  lvl2a.toList.sortBy(_.getClass.getSimpleName).foreach(println)
  println("----")

def getGate(gates: List[Gate], name: String): Set[Gate] =
  gates.filter(g => g.regNameL == name || g.regNameR == name).toSet

def getHashKey(gateMap: Map[String, Gate], output: String): String =
  if !gateMap.contains(output) then output
  else
    val gate = gateMap(output)
    val key1 = getHashKey(gateMap, gate.regNameL)
    val key2 = getHashKey(gateMap, gate.regNameR)
    s"${gate.getClass.getSimpleName}($key1, $key2)"

def parseInput(list: List[String]) =
  val dataList = list.head.split("\n").map(_.trim).toList
  val gateList = list.last.split("\n").map(_.trim).toList
  val data = dataList.map(_.split(": ").toList).map(a => Register(a.head, a.last.toInt))
  val gates = gateList.map(_.split(" -> ").toList).map(a => toGate(a.head.split(" ").toList, a.last))
  (data, gates)

def toGate(data: List[String], resRegister: String) = data match
  case List(a, "AND", b) => And(a, b, resRegister)
  case List(a, "OR", b) => Or(a, b, resRegister)
  case List(a, "XOR", b) => Xor(a, b, resRegister)
  case _ => throw new IllegalArgumentException(s"Invalid input $data, $resRegister")

@main
def main(): Unit =
  val input = Source.fromResource("day24.txt").mkString.split("\n\n").map(_.trim).toList
  val (inputs, gates) = parseInput(input)
  println(s"pt1: ${pt1(inputs, gates)}")
  println(s"pt2: ${pt2(gates)}")
