package part.one

import util.AOCUtil._

object TaskOne {

  def main(args: Array[String]): Unit = println(calculate(fileName = "/input.txt"))

  private def calculate(fileName: String): Int = {
    val fileAsLines          = readAndTraverseFile(fileName)
    val rightSide            = fileAsLines.map(getBothSides)
    val uniquelySizedNumbers = Set(2, 3, 4, 7)
    rightSide.flatMap(_._2.map(_.length)).count(uniquelySizedNumbers)
  }
}
