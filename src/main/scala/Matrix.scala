import Matrix.Direction.*
import Matrix.{Direction, Elem}

import scala.annotation.tailrec

class Matrix[A](elements: Array[Array[Elem[A]]]) extends Iterable[Elem[A]] with MatrixOps[A] {
  def elemAt(row: Int, col: Int): Option[Elem[A]] = {
    elements.lift(row).flatMap(_.lift(col))
  }

  def update(row: Int, col: Int, value: A): Elem[A] = {
    val newElem = Elem(value, row, col)
    elements(row).update(col, newElem)
    newElem
  }

  override def iterator: Iterator[Elem[A]] = new scala.collection.AbstractIterator[Elem[A]] {
    private val _iterator: Iterator[Iterator[Elem[A]]] = elements.iterator.map(_.iterator)

    private var _currentRow: Iterator[Elem[A]] =
      if (_iterator.hasNext) _iterator.next() else Iterator.empty[Elem[A]]

    override def hasNext: Boolean =
      _currentRow.hasNext || hasNextRow

    override def next(): Elem[A] = {
      if (!_currentRow.hasNext) {
        nextRow()
      }
      _currentRow.next()
    }

    private def hasNextRow: Boolean = _iterator.hasNext

    private def nextRow(): Iterator[Elem[A]] = {
      _currentRow = _iterator.next()
      _currentRow
    }
  }

  override def toString(): String = elements.map(_.mkString).mkString("\n")
}

object Matrix {
  def apply[A](source: Array[Array[A]]): Matrix[A] = {
    new Matrix(
      source.zipWithIndex.map((rowValues, row) => rowValues.zipWithIndex.map((value, col) => Elem[A](value, row, col)))
    )
  }

  case class Elem[A](value: A, row: Int, col: Int) {
    override def toString: String = value.toString
    def info: String = s"$value ($row, $col)"
  }

  enum Direction(val step: (Int, Int) => (Int, Int)) {
    case UpLeft extends Direction((r, c) => (r - 1, c - 1))
    case Up extends Direction((r, c) => (r - 1, c))
    case UpRight extends Direction((r, c) => (r - 1, c + 1))
    case Left extends Direction((r, c) => (r, c - 1))
    case Right extends Direction((r, c) => (r, c + 1))
    case DownLeft extends Direction((r, c) => (r + 1, c - 1))
    case Down extends Direction((r, c) => (r + 1, c))
    case DownRight extends Direction((r, c) => (r + 1, c + 1))
  }
}

trait MatrixOps[A] {
  self: Matrix[A] =>

  def nextElem(row: Int, col: Int, direction: Direction): Option[Elem[A]] = {
    self.elemAt.tupled(direction.step(row, col))
  }

  def nextElem(el: Elem[A], direction: Direction): Option[Elem[A]] = {
    self.elemAt.tupled(direction.step(el.row, el.col))
  }

  def elementsAt(row: Int, col: Int, direction: Direction, n: Int): Option[List[Elem[A]]] = {
    self.elemAt(row, col).flatMap(el => elementsAt(el, direction, n))
  }

  def elementsAt(el: Elem[A], direction: Direction, n: Int): Option[List[Elem[A]]] = {
    elementsAtAux(Some(el), direction, n, List.empty[Elem[A]])
  }

  @tailrec
  private def elementsAtAux(
      el: Option[Elem[A]],
      direction: Direction,
      n: Int,
      acc: List[Elem[A]]
  ): Option[List[Elem[A]]] = {
    if (n > 0) {
      el match {
        case Some(el) =>
          elementsAtAux(nextElem(el, direction), direction, n - 1, acc :+ el)
        case _ => None
      }
    } else {
      Some(acc)
    }
  }
}
