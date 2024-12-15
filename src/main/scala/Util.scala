import scala.io.Source
import scala.reflect.ClassTag

object Util {
  def readFile(filename: String): List[String] = {
    val bufferedSource = Source.fromFile(filename)
    val result = bufferedSource.getLines.toList
    bufferedSource.close
    result
  }

  def read[A: ClassTag](filename: String, delim: String = """\s+""")(parse: String => A = identity): List[List[A]] = {
    readFile(filename)
      .map(line => line.split(delim).map(it => parse(it)).toList)
  }

  def foldWhile[B](z: B)(op: PartialFunction[B, B]): B = {
    var acc = z
    while (op.isDefinedAt(acc)) {
      acc = op(acc)
    }
    acc
  }

  extension [A](iterable: IterableOnce[A]) {
    def foldLWhile[B](z: B)(op: PartialFunction[(B, A), B]): B = {
      scala.util.boundary {
        iterable.iterator.foldLeft(z) { case b @ (acc, _) =>
          if (op.isDefinedAt(b)) op(b) else scala.util.boundary.break(acc)
        }
      }
    }
  }
}
