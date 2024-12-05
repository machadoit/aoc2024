import Util.readFile

@main def day03(): Unit = {

  val input = readFile("resources/day03")

  // Part 1

  val muls: List[Mul] = parseMuls(input.mkString)
  val result1 = muls.foldLeft(0)((acc, mul) => acc + mul.result)

  println(result1)

  // Part 2

  val validMuls: List[Mul] = parseValidMuls(input.mkString)
  val result2 = validMuls.foldLeft(0)((acc, mul) => acc + mul.result)

  println(result2)
}

case class Mul(x: Int, y: Int) {
  val result: Int = x * y
}

def parseMuls(line: String): List[Mul] = {
  """mul\((\d+),(\d+)\)""".r
    .findAllMatchIn(line)
    .map { mul =>
      Mul(mul.subgroups.head.toInt, mul.subgroups.last.toInt)
    }
    .toList
}

def parseValidMuls(line: String): List[Mul] = {
  validSubstrings(line).flatMap(parseMuls)
}

def validSubstrings(line: String): List[String] = {
  val paddedLine = s"""do()${line}don't()"""

  """do\(\)(.*?)don't\(\)""".r
    .findAllMatchIn(paddedLine)
    .map(_.subgroups.head)
    .toList
}
