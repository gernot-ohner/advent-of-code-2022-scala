package ohner.dev

import scala.io.Source

object Main {

  def main(args: Array[String]): Unit = {
    val day = 4
    val (testInput, prodInput) = readFilesForDay(day)
    val solution = getSolutionForDay(day)

    val testResultTask1 = solution.task1(testInput)
    println(s"TEST result: $testResultTask1")
    val prodResultTask1 = solution.task1(prodInput)
    println(s"PROD result: $prodResultTask1")

    val testResultTask2 = solution.task2(testInput)
    println(s"TEST result: $testResultTask2")
    val prodResultTask2 = solution.task2(prodInput)
    println(s"PROD result: $prodResultTask2")

  }

  private def readFilesForDay(day: Int) = (readFileForDay(day, "TEST"), readFileForDay(day, "PROD"))

  private def readFileForDay(day: Int, env: String) = {
    val source = Source.fromFile(s"/Users/gernotohner/dev/advent-of-code-2022/src/main/resources/day$day/$env.in")
    val lines = source.getLines().toList
    source.close()
    lines
  }

  // Is there a way to do this programmatically?
  // Would probably necessitate reflection
  def getSolutionForDay(day: Int): Solution[?] = {
    day match
      case 1 => Day1
      case 2 => Day2
      case 3 => Day3
      case 4 => Day4
      case 5 => Day5
      case 6 => Day6
      case 7 => Day7
  }


}
