import Util.read

import scala.annotation.tailrec

@main def day11(): Unit = {

  val input: List[String] = read("resources/day11")().head

  // Part 1

  val result1 = blink(input, 25).length

  println(result1)

  // Part 2

  val result2 = "foo"

  println(result2)
}

@tailrec
def blink(stones: List[String], n: Int): List[String] = {
  if (n > 0) {
    blink(stones.flatMap(stone => blinkStone(stone)), n - 1)
  } else {
    stones
  }
}

def blinkStone(stone: String): List[String] = {
  stone match {
    case "0" =>
      List("1")
    case stone if stone.length % 2 == 0 =>
      val (left, right) = stone.splitAt(stone.length / 2)
      List(left.toInt.toString, right.toInt.toString)
    case stone =>
      List((stone.toLong * 2024).toString)
  }
}
