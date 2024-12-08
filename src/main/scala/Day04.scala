import Matrix.Direction.*
import Matrix.{Direction, Element}
import Util.readFile

@main def day04(): Unit = {

  type Puzzle = Matrix[Char, Element]
  given puzzle: Puzzle = Matrix(readFile("resources/day04").map(_.toCharArray).toArray)

  // Part 1

  val words = puzzle.flatMap(el => wordsAt(el))
  val result1 = words.count(isXmas)

  println(result1)

  // Part 2

  val result2 = puzzle.count(el => isCrossXmas(el))

  println(result2)
}

def wordsAt(elem: Element[Char])(using puzzle: Matrix[Char, Element]): List[String] = {
  List(Down, Right, DownLeft, DownRight).flatMap { direction =>
    puzzle.elementsAt(elem, direction, "XMAS".length).map(_.mkString)
  }
}

def isCrossXmas(el: Element[Char])(using puzzle: Matrix[Char, Element]): Boolean = {
  (for {
    upLeft <- puzzle.nextElem(el, UpLeft)
    upRight <- puzzle.nextElem(el, UpRight)
    center = el
    downLeft <- puzzle.nextElem(el, DownLeft)
    downRight <- puzzle.nextElem(el, DownRight)
  } yield {
    isMas(Array(upLeft, center, downRight).mkString) && isMas(Array(downLeft, center, upRight).mkString)
  }).getOrElse(false)
}

def isXmas(s: String): Boolean = {
  s == "XMAS" || s.reverse == "XMAS"
}

def isMas(s: String): Boolean = {
  s == "MAS" || s.reverse == "MAS"
}
