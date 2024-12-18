package day17

import scala.annotation.tailrec
import scala.io.Source

case class Registers(a: Long, b: Long, c: Long)

def pt1(registers: Registers, program: List[Int]): String =
  val outputs = runInstruction(registers, program, 0)
  outputs.mkString(",")

def pt2(registers: Registers, program: List[Long]): String =
  var found: List[Long] = List()
  var searching = true
  var a = BigInt(8).pow(15)
  while searching do
    val outputs = pt22(a, program)
    if outputs.size > found.size then
      found = outputs(found.size) :: found
      println(s"Outputs: $outputs at $a")
      a = a + 1
    else
      a = a + 1
    if outputs.length == program.length then
      searching = false
  a.toString()


def pt22(aIn: BigInt, program: List[Long]): List[Long] =
  var outputs: List[Long] = List()
  var searching = true
  var a = aIn
  while searching do
    var b = a % 8 ^ 3 // b är mellan 0 - 7
    val c = a / Math.pow(2, b.toLong).toLong // c = a / 2^b // nämnare är max 128
    b = b ^ c ^ 3 // br xor 3 xor c = bin // b ^ c måste vara en multipel av 8 + 3, //b + 3 + c måste vara en multipel av 8
    a = a / 8
    val res = (b % 8).toLong  // för att skriva ut 0, måste B vara en multipel av 8 här
    if program(outputs.length) != res then
      searching = false
    else if a == 0 then
      println(outputs)
      searching = false
      outputs = outputs :+ res
    else
      outputs = outputs :+ res
  outputs  

@tailrec
def runInstruction(registers: Registers, program: List[Int], pointer: Int, outputs: List[Int] = List()): List[Int] =
  if pointer >= program.length then
    outputs
  else
    val operand = program(pointer + 1)
    program(pointer) match
      case 0 => // adv
        val operandValue = getOperand(registers, operand)
        val denominator = Math.pow(2, operandValue).toLong
        val result = registers.a / denominator
        runInstruction(registers.copy(a = result), program, pointer + 2, outputs)
      case 1 => // bxl
        val result = registers.b ^ operand
        runInstruction(registers.copy(b = result), program, pointer + 2, outputs)
      case 2 => // bst
        val operandValue = getOperand(registers, operand)
        val result = operandValue % 8
        runInstruction(registers.copy(b = result), program, pointer + 2, outputs)
      case 3 => // jnz
        if registers.a != 0 then
          runInstruction(registers, program, operand, outputs)
        else
          runInstruction(registers, program, pointer + 2, outputs)
      case 4 => // bxc
        val result = registers.b ^ registers.c
        runInstruction(registers.copy(b = result), program, pointer + 2, outputs)
      case 5 => // out
        val operandValue = getOperand(registers, operand)
        val result = operandValue % 8
        runInstruction(registers, program, pointer + 2, outputs :+ result.toInt)
      case 6 => // bdv
        val operandValue = getOperand(registers, operand)
        val denominator = Math.pow(2, operandValue)
        val result = registers.a / denominator
        runInstruction(registers.copy(b = result.toLong), program, pointer + 2, outputs)
      case 7 => // cdv
        val operandValue = getOperand(registers, operand)
        val denominator = Math.pow(2, operandValue)
        val result = registers.a / denominator
        runInstruction(registers.copy(c = result.toLong), program, pointer + 2, outputs)
      case _ => throw new IllegalArgumentException(s"Invalid opcode: ${program(pointer)} at $pointer")

def getOperand(registers: Registers, operand: Long): Long = operand match
  case 0 | 1 | 2 | 3 => operand
  case 4 => registers.a
  case 5 => registers.b
  case 6 => registers.c
  case _ => throw new IllegalArgumentException(s"Invalid operand: $operand")

def isValidSolution(a: Long, program: List[Int]): Boolean =
  val resPt1 = pt1(Registers(a, 0, 0), program.reverse)
  resPt1 == program.reverse.mkString(",")

def pt2(program: List[Int], currNum: Long = 0, nextI: Int = 0, outputs: List[Int] = List(), pos: Int = 0): List[Long] =
  if nextI > 7 then
    Nil
  else if outputs.length == program.length && isValidSolution(currNum, program) then
    println(outputs)
    currNum :: Nil
  else
    val a = currNum + nextI
    var b = a % 8 ^ 3
    val c = a / Math.pow(2, b.toInt).toInt
    b = b ^ c ^ 3
    val res = b % 8
    if res != program(outputs.length) then
      pt2(program, currNum, nextI + 1, outputs, pos)
    else if (pos + 1) == program.length then
      a :: Nil
    else
      val doneOutputs = pt2(program, a * 8, 0, outputs :+ res.toInt, pos + 1)
      if doneOutputs.nonEmpty && isValidSolution(doneOutputs.head, program) then
        doneOutputs
      else
        pt2(program, currNum, nextI + 1, outputs, pos)


def parseInput(line: List[String]): (Registers, List[Int]) =
  val registersString = line.head.split("\n").map(_.trim).filterNot(_.isBlank).toList
  val rA = registersString.head.split(":").last.trim.toLong
  val rB = registersString(1).split(":").last.trim.toLong
  val rC = registersString(2).split(":").last.trim.toLong

  val program = line(1).split(":").last.trim.split(",").map(_.trim).map(_.toInt).toList
  (Registers(rA, rB, rC), program)

@main
def main(): Unit =
  val input = Source.fromResource("day17.txt").mkString.split("\n\n").map(_.trim).filterNot(_.isBlank).toList
  val (registers, program) = parseInput(input)
  println(s"pt1: ${pt1(registers, program)}")
  println(s"pt2: ${pt2(program.reverse).head}")

