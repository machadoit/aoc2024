import Util.{foldWhile, readFile}
import Direction.*

@main def day04(): Unit = {

  given puzzle: Puzzle = Puzzle(readFile("resources/day04").map(_.toCharArray).toArray)

  // Part 1

  val words = (for {
    row <- puzzle.rowIndices
    col <- puzzle.colIndices
  } yield wordsAt(row, col)).flatten

  val result1 = words.count(isXmas)

  println(result1)

  // Part 2

  val crosses = for {
    row <- puzzle.rowIndices
    col <- puzzle.colIndices
  } yield {
    isCrossXmas(row, col)
  }
  val result2 = crosses.count(identity)

  println(result2)
}

def wordsAt(row: Int, col: Int)(using puzzle: Puzzle): List[String] = {
  List(Down, Right, DownLeft, DownRight).flatMap { direction =>
    puzzle.wordAt(row, col, direction, "XMAS".length)
  }
}

def isCrossXmas(row: Int, col: Int)(using puzzle: Puzzle): Boolean = {
  def charAt(direction: Direction) = {
    val (r, c) = direction.step(row, col)
    puzzle(r, c)
  }

  (for {
    center <- charAt(Center)
    upLeft <- charAt(UpLeft)
    upRight <- charAt(UpRight)
    downLeft <- charAt(DownLeft)
    downRight <- charAt(DownRight)
  } yield {
    isMas(Array(upLeft, center, downRight).mkString) && isMas(Array(downLeft, center, upRight).mkString)
  }).getOrElse(false)
}

case class Puzzle(input: Array[Array[Char]]) {
  val rowIndices: List[Int] = input.indices.toList
  val colIndices: List[Int] = input.head.indices.toList

  def apply(row: Int, col: Int): Option[Char] = {
    input.lift(row).flatMap(_.lift(col))
  }

  def wordAt(fromRow: Int, fromCol: Int, direction: Direction, length: Int): Option[String] = {
    val (word, _, _, _) = foldWhile(Option(List.empty[Char]), fromRow, fromCol, length) {
      case (Some(res), row, col, remaining) if remaining > 0 =>
        val (nextRow, nextCol) = direction.step(row, col)
        (apply(row, col).map(res.appended), nextRow, nextCol, remaining - 1)
    }
    word.map(_.mkString)
  }
}

enum Direction(val step: (Int, Int) => (Int, Int)) {
  case Center extends Direction((r, c) => (r, c))
  case Down extends Direction((r, c) => (r + 1, c))
  case Right extends Direction((r, c) => (r, c + 1))
  case DownLeft extends Direction((r, c) => (r + 1, c - 1))
  case DownRight extends Direction((r, c) => (r + 1, c + 1))
  case UpLeft extends Direction((r, c) => (r - 1, c - 1))
  case UpRight extends Direction((r, c) => (r - 1, c + 1))
}

def isXmas(s: String): Boolean = {
  s == "XMAS" || s.reverse == "XMAS"
}

def isMas(s: String): Boolean = {
  s == "MAS" || s.reverse == "MAS"
}
