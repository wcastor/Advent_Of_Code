package part.two

import util.AOCUtil._
import util.NumberOfSegments._

object TaskTwo {

  private def intersectionSearchCount(knownCharSet: Set[Char], word: Word): Int =
    (word.toSet & knownCharSet).size

  private def convertSegmentToNumber(four: Set[Char], seven: Set[Char])(word: Word): Int =
    (word.length, intersectionSearchCount(four, word), intersectionSearchCount(seven, word)) match {
      case (Zero, 3, 3)  => 0
      case (One, _, _)   => 1
      case (Two, 2, 2)   => 2
      case (Three, 3, 3) => 3
      case (Four, _, _)  => 4
      case (Five, 3, 2)  => 5
      case (Six, 3, 2)   => 6
      case (Seven, _, _) => 7
      case (Eight, _, _) => 8
      case (Nine, 4, 3)  => 9
      case _             => -1
    }

  private def uniqueSetOfCharsBySize(inputString: List[Word], length: Int): Set[Char] =
    inputString.find(_.length == length).toSet.flatten

  private val calculateCodeSum = (inputString: Line) => {
    val (leftSide, rightSide) = getBothSides(inputString)
    val four                  = uniqueSetOfCharsBySize(leftSide, Four)
    val seven                 = uniqueSetOfCharsBySize(leftSide, Seven)
    val semiResult            = rightSide.map(convertSegmentToNumber(four, seven))
    semiResult.find(_ == -1).map(_ => 0).getOrElse(semiResult.foldLeft(0)(_ * 10 + _))
  }

  private val calculate = (_: List[Line]).map(calculateCodeSum).sum

  def main(args: Array[String]): Unit = println(calculate(readAndTraverseFile("/input.txt")))
}
