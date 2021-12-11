package util

import scala.io.Source
import scala.util.Try

object AOCUtil {

  type Word = String
  type Line = String

  def readAndTraverseFile: String => List[Line] = { fileName =>
    Try {
      val path      = getClass.getResource(fileName).getPath
      val wholeFile = Source.fromFile(path)
      val lines     = wholeFile.getLines().toList
      wholeFile.close()
      lines
    }.getOrElse(Nil)
  }

  private val trimAndSplit = (_: Line).trim.split(" ").toList

  val getBothSides: Line => (List[Word], List[Word]) =
    _.split("\\|").toList match {
      case first :: second :: Nil => (trimAndSplit(first), trimAndSplit(second))
      case _                   => (Nil, Nil)
    }
}
