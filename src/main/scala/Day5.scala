package ohner.dev

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day5 extends Solution[String] {

  private val regex = """move (\d+) from (\d+) to (\d+)""".r

  override def task1(input: List[String]): String = {
    val p = parseInput(input)
    val stacks = p.startingStacks
    p.instructions.foreach(i => {
      (0 until i.numToMove).foreach(_ => {
        val element = stacks(i.fromStack).pop()
        stacks(i.toStack).push(element)
      })
    })

    stacks.map(_.pop()).mkString
  }

  override def task2(input: List[String]): String = {
    val p = parseInput(input)
    val stacks = p.startingStacks
    p.instructions.foreach(i => {
      (0 until i.numToMove).map(_ => { stacks(i.fromStack).pop() })
        .reverse
        .foreach(crate => stacks(i.toStack).push(crate))
    })
    stacks.map(_.pop()).mkString
  }

  private def parseInput(input: List[String]) = {
    val startingPositionString = input.takeWhile(s => s != "")
    val startingPosition = parseStartingPositions(startingPositionString.dropRight(1))
    val instructions = parseInstructions(input.drop(startingPositionString.length + 1))
    RearrangementProcedure(startingPosition, instructions)
  }

  def parseStartingPositions(input: List[String]): Stacks = {
    input.transpose
      .map(line => line.filter(_.isUpper))
      .filter(_.nonEmpty)
      .map(line => mutable.Stack.from(line))

  }

  def parseInstructions(input: List[String]): List[Instruction] = {
    input.map(line => line match
      case regex(numToMove, fromStack, toStack) =>
        Instruction(numToMove.toInt, fromStack.toInt -1, toStack.toInt - 1))
  }
}

type Stacks = List[mutable.Stack[Char]]

case class RearrangementProcedure(startingStacks: Stacks, instructions: List[Instruction])

case class Instruction(numToMove: Int, fromStack: Int, toStack: Int)
