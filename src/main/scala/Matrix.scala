import Matrix.Direction.*
import Matrix.{Direction, Element}

import scala.annotation.tailrec

class Matrix[A, Elem[AA] <: Element[AA]](elements: Array[Array[Elem[A]]]) extends Iterable[Elem[A]] {
  def elemAt(row: Int, col: Int): Option[Elem[A]] = {
    elements.lift(row).flatMap(_.lift(col))
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

  def nextElem(el: Elem[A], direction: Direction): Option[Elem[A]] = {
    elemAt.tupled(direction.step(el.row, el.col))
  }

  def elementsAt(row: Int, col: Int, direction: Direction, n: Int): Option[List[Elem[A]]] = {
    elemAt(row, col).flatMap(el => elementsAt(el, direction, n))
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

object Matrix {
  def apply[A](source: Array[Array[A]]): Matrix[A, Element] = {
    custom[A, Element](source)((_value, _row, _col) => {
      new Element[A] {
        override val value: A = _value
        override val row: Int = _row
        override val col: Int = _col
      }
    })
  }

  def custom[A, Elem[AA] <: Element[AA]](
      source: Array[Array[A]]
  )(elemBuilder: (A, Int, Int) => Elem[A]): Matrix[A, Elem] = {
    new Matrix[A, Elem](
      source.zipWithIndex.map((rowValues, row) =>
        rowValues.zipWithIndex.map((value, col) => elemBuilder(value, row, col))
      )
    )
  }

  trait Element[A] {
    def value: A
    def row: Int
    def col: Int

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
