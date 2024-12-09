import Matrix.Elem
import Util.readFile

@main def day08(): Unit = {

  given puzzle: Matrix[Char] = Matrix(readFile("resources/day08").map(_.toCharArray).toArray)

  // Part 1

  val allAntennas = puzzle.groupBy(_.value).filterNot(_._1 == '.')

  val uniqueAntinodes1 = allAntennas.values.toSet.flatMap(antennas => antinodes(antennas.toList, part1 = true))

  val result1 = uniqueAntinodes1.size

  println(result1)

  // Part 2
  val uniqueAntinodes = allAntennas.values.toSet.flatMap(antennas => antinodes(antennas.toList, part1 = false))

  val result2 = uniqueAntinodes.size

  println(result2)
}

def antinodes(antennas: List[Elem[Char]], part1: Boolean)(using puzzle: Matrix[Char]): Set[Elem[Char]] = {
  antennas
    .combinations(2)
    .flatMap {
      case first :: second :: Nil => antinodes(first, second, part1)
      case _                      => throw new RuntimeException("Review code")
    }
    .toSet
}

def antinodes(antenna1: Elem[Char], antenna2: Elem[Char], part1: Boolean)(using
    puzzle: Matrix[Char]
): Set[Elem[Char]] = {
  def anti(axis: Elem[Char] => Int) = {
    val diff = Math.abs(axis(antenna1) - axis(antenna2))

    if (axis(antenna1) <= axis(antenna2)) {
      (itDec(axis(antenna1), diff), itSum(axis(antenna2), diff))
    } else (itSum(axis(antenna1), diff), itDec(axis(antenna2), diff))
  }

  def itSum(start: Int, diff: Int) = {
    Iterator.iterate(start)(_ + diff)
  }
  def itDec(start: Int, diff: Int) = {
    Iterator.iterate(start)(_ - diff)
  }

  def nodeIterator(rowIt: Iterator[Int], colIt: Iterator[Int]): Iterator[Option[Elem[Char]]] = {
    rowIt.zip(colIt).map { case (row, col) => puzzle.elemAt(row, col) }
  }

  val (rowA, rowB) = anti(_.row)
  val (colA, colB) = anti(_.col)

  val iteratorA = nodeIterator(rowA, colA)
  val iteratorB = nodeIterator(rowB, colB)

  if (part1) {
    (iteratorA.drop(1).next() ++ iteratorB.drop(1).next()).toSet
  } else {
    (validNodes(iteratorA) ++ validNodes(iteratorB)).toSet
  }
}

def validNodes(nodeIterator: Iterator[Option[Elem[Char]]]): Iterator[Elem[Char]] = {
  nodeIterator.takeWhile(_.isDefined).flatten
}
