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
  println(s"pt2: ${}")

val example =
  """
    |x00: 1
    |x01: 1
    |x02: 1
    |y00: 0
    |y01: 1
    |y02: 0
    |
    |x00 AND y00 -> z00
    |x01 XOR y01 -> z01
    |x02 OR y02 -> z02
    |""".stripMargin.split("\n\n").map(_.trim).toList

val example2 =
  """
    |x00: 1
    |x01: 0
    |x02: 1
    |x03: 1
    |x04: 0
    |y00: 1
    |y01: 1
    |y02: 1
    |y03: 1
    |y04: 1
    |
    |ntg XOR fgs -> mjb
    |y02 OR x01 -> tnw
    |kwq OR kpj -> z05
    |x00 OR x03 -> fst
    |tgd XOR rvg -> z01
    |vdt OR tnw -> bfw
    |bfw AND frj -> z10
    |ffh OR nrd -> bqk
    |y00 AND y03 -> djm
    |y03 OR y00 -> psh
    |bqk OR frj -> z08
    |tnw OR fst -> frj
    |gnj AND tgd -> z11
    |bfw XOR mjb -> z00
    |x03 OR x00 -> vdt
    |gnj AND wpb -> z02
    |x04 AND y00 -> kjc
    |djm OR pbm -> qhw
    |nrd AND vdt -> hwm
    |kjc AND fst -> rvg
    |y04 OR y02 -> fgs
    |y01 AND x02 -> pbm
    |ntg OR kjc -> kwq
    |psh XOR fgs -> tgd
    |qhw XOR tgd -> z09
    |pbm OR djm -> kpj
    |x03 XOR y03 -> ffh
    |x00 XOR y04 -> ntg
    |bfw OR bqk -> z06
    |nrd XOR fgs -> wpb
    |frj XOR qhw -> z04
    |bqk OR frj -> z07
    |y03 OR x01 -> nrd
    |hwm AND bqk -> z03
    |tgd XOR rvg -> z12
    |tnw OR pbm -> gnj
    |""".stripMargin.split("\n\n").map(_.trim).toList