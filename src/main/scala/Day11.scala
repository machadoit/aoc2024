import Util.*

import scala.collection.mutable

@main def day11(): Unit = {

  val input: List[BigInt] = read("resources/day11")(BigInt.apply).head

  // Part 1

  val result1 = Tree(input, 25).solve()

  println(result1)

  // Part 2

  val result2 = Tree(input, 75).solve()

  println(result2)
}

class Tree(roots: List[BigInt], totalBlinks: Int) {
  private val memory = new Memory
  private var total: BigInt = 0

  def solve(): BigInt = {
    foldWhile(roots.map((_, totalBlinks, List.empty[(BigInt, Int)]))) {
      case (stone, blinks, path) :: rest if memory.scores.get(stone, blinks).isDefined || blinks == 1 =>
        total += memory.scores.getOrElseUpdate((stone, blinks), blinkStone(stone).length)
        memory.decPath(path)
        rest
      case (stone, blinks, path) :: rest =>
        val nextStones = blinkStone(stone).map { nextStone =>
          val nextPath = (stone, blinks) :: path
          memory.upPath(nextPath)
          (nextStone, blinks - 1, nextPath)
        }
        memory.decPath(path)
        nextStones ::: rest
    }
    total
  }

  private def blinkStone(stone: BigInt): List[BigInt] = {
    memory.stones.getOrElseUpdate(
      stone,
      stone.toString match {
        case "0" =>
          List(BigInt(1))
        case str if str.length % 2 == 0 =>
          val (left, right) = str.splitAt(str.length / 2)
          List(BigInt(left), BigInt(right))
        case _ =>
          List(stone * 2024)
      }
    )
  }
}

class Memory {
  val stones: mutable.Map[BigInt, List[BigInt]] = mutable.Map.empty
  val scores: mutable.Map[(BigInt, Int), BigInt] = mutable.Map.empty

  private val pendingPaths: mutable.Map[(BigInt, Int), Int] = mutable.Map.empty

  def decPath(path: List[(BigInt, Int)]): Unit = {
    path.foreach { p =>
      val newValue = pendingPaths(p) - 1
      pendingPaths.update(p, newValue)
      if (newValue == 0) triggerScoring.tupled(p)
    }
  }

  def upPath(path: List[(BigInt, Int)]): Unit = {
    path.foreach(p => pendingPaths.update(p, pendingPaths.getOrElse(p, 0) + 1))
  }

  private def triggerScoring(stone: BigInt, blinks: Int): Unit = {
    getAndUpdate(stone, blinks)
  }

  private def getAndUpdate(stone: BigInt, blinks: Int): BigInt = {
    if (blinks == 1) {
      scores.getOrElseUpdate((stone, blinks), stones(stone).length)
    } else {
      scores.getOrElseUpdate((stone, blinks), stones(stone).map(getAndUpdate(_, blinks - 1)).sum)
    }
  }
}
