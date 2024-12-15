import Util.*

@main def day11(): Unit = {

  val input: List[BigInt] = read("resources/day11")(BigInt.apply).head

  // Part 1

  val result1 = Tree(input, 25).solve()

  println(result1)

  // Part 2

  val result2 = "foo"

  println(result2)
}

class Tree(roots: List[BigInt], totalBlinks: Int) {
  var total: BigInt = 0

  def solve(): BigInt = {
    foldWhile(roots.map((_, totalBlinks))) {
      case (stone, 1) :: rest =>
        total += blinkStone(stone).length
        rest
      case (stone, blinks) :: rest =>
        blinkStone(stone).map((_, blinks - 1)) ::: rest
    }
    total
  }

  private def blinkStone(stone: BigInt): List[BigInt] = {
    stone.toString match {
      case "0" =>
        List(BigInt(1))
      case str if str.length % 2 == 0 =>
        val (left, right) = str.splitAt(str.length / 2)
        List(BigInt(left), BigInt(right))
      case _ =>
        List(stone * 2024)
    }
  }
}
