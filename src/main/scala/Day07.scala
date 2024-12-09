import Util.read

@main def day07(): Unit = {

  val delim = """:?\s+"""
  val input = read("resources/day07", delim)(_.toLong)

  // Part 1

  val result1 = input.flatMap(line => result(line)(using new Part1)).sum

  println(result1)

  // Part 2

  val result2 = input.flatMap(line => result(line)(using new Part2)).sum

  println(result2)
}

def result(equation: List[Long])(using algebra: Algebra): Option[Long] = {
  equation match
    case total :: numbers if totals(numbers).contains(total) => Some(total)
    case _                                                   => None
}

def totals(numbers: List[Long])(using algebra: Algebra): List[Long] = {
  numbers match
    case first :: rest =>
      algebra.math(rest, first)
    case Nil => List.empty[Long]
}

trait Algebra {
  def math(numbers: List[Long], value: Long): List[Long]
}

class Part1 extends Algebra {
  override def math(numbers: List[Long], value: Long): List[Long] = {
    numbers match
      case first :: rest =>
        math(rest, value + first) ++ math(rest, value * first)
      case rest =>
        List(value)
  }
}

class Part2 extends Algebra {
  override def math(numbers: List[Long], value: Long): List[Long] = {
    numbers.foldLeft(List(value)) { (results, n) =>
      results.flatMap(r => List(r + n, r * n, (r.toString ++ n.toString).toLong))
    }
  }
}
