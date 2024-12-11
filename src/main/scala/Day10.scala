import Matrix.{Direction, Elem}
import Util.{foldWhile, read}

@main def day10(): Unit = {

  given matrix: Matrix[Int] = Matrix(read("resources/day10", delim = "")(_.toInt))

  // Part 1
  val result1 = trails().map(_.toSet.size).sum

  println(result1)

  // Part 2
  val result2 = trails().map(_.length).sum

  println(result2)
}

def trails()(using matrix: Matrix[Int]): List[List[Elem[Int]]] = {
  matrix.filter(_.value == 0).toList.map { trail =>
    val (_, nines) = foldWhile(List(trail), List.empty[Elem[Int]]) {
      case ((el @ Elem(9, _, _)) :: rest, nines) =>
        (rest, el +: nines)
      case (el :: rest, nines) =>
        (next(el) ::: rest, nines)
    }
    nines
  }
}

private def next(el: Elem[Int])(using matrix: Matrix[Int]): List[Elem[Int]] = {
  List(Direction.Up, Direction.Down, Direction.Left, Direction.Right).flatMap { direction =>
    matrix.nextElem(el, direction).filter(_.value == el.value + 1)
  }
}
