import Matrix.{Direction, Elem}
import Util.readFile

import scala.annotation.tailrec

@main def day06(): Unit = {

  val input = readFile("resources/day06").map(_.toCharArray).toArray
  val puzzle1 = Puzzle(Matrix(input))
  // Part 1
  puzzle1.move()
  val result1 = puzzle1.count(_.value == 'X')

  println(result1)

  // Part 2
  val result2 = Matrix(input).foldLeft(0) { case (loops, possibleNewWall) =>
    val puzzle = new Puzzle(Matrix(input))
    puzzle.setWall(possibleNewWall) match {
      case Some(_) => loops + puzzle.move().value
      case None    => loops
    }
  }

  println(result2)
}

enum Result(val value: Int):
  case End extends Result(0)
  case Loop extends Result(1)

class Puzzle(val matrix: Matrix[Char]) {
  export matrix._

  private val startingGuard = matrix.collectFirst { case Elem('^', row, col) => Elem(Direction.Up, row, col) }.get

  def move(): Result = {
    moveAux(startingGuard, matrix.size) match
      case result: Result => result
      case _              => throw new RuntimeException("Review code")
  }

  @tailrec
  private def moveAux(guard: Elem[Direction], movesLeft: Int): Result | Elem[Direction] = {
    matrix.update(guard.row, guard.col, 'X')
    matrix.nextElem(guard.row, guard.col, guard.value) match {
      case Some(Elem('#', _, _)) =>
        moveAux(rotate(guard), movesLeft)
      case Some(Elem(_, nextRow, nextCol)) if movesLeft > 0 =>
        moveAux(guard.copy(row = nextRow, col = nextCol), movesLeft - 1)
      case Some(_) if movesLeft <= 0 =>
        Result.Loop
      case _ =>
        Result.End
    }
  }

  private def rotate(guard: Elem[Direction]): Elem[Direction] = {
    val newDirection = guard.value match {
      case Direction.Up    => Direction.Right
      case Direction.Right => Direction.Down
      case Direction.Down  => Direction.Left
      case Direction.Left  => Direction.Up
    }
    guard.copy(value = newDirection)
  }

  def setWall(el: Elem[Char]): Option[Elem[Char]] = {
    el.value match {
      case '.' => Some(update(el.row, el.col, '#'))
      case _   => None
    }
  }
}
