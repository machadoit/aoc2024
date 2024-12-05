import Util.*

@main def day05(): Unit = {
  val lines = readFile("resources/day05")
  val rules = lines.takeWhile(_.nonEmpty).map(_.split('|').map(_.toInt))
  val updates = lines.dropWhile(_.nonEmpty).dropWhile(_.isEmpty).map(_.split(',').toList.map(_.toInt))

  val beforeLookup: Map[Int, Set[Int]] = rules.groupBy(_.head).map((k, values) => k -> values.map(_.last).toSet)
  val notBeforeLookup: Map[Int, Set[Int]] = rules.groupBy(_.last).map((k, values) => k -> values.map(_.head).toSet)

  // Part 1

  val updateResults = updates.flatMap(update => validUpdate(update)(beforeLookup, notBeforeLookup))
  val result1 = updateResults.sum

  println(result1)

  // Part 2

  val invalidResults = updates.flatMap(update => invalidUpdate(update)(beforeLookup, notBeforeLookup))
  val result2 = invalidResults.sum

  println(result2)
}

def validUpdate(
    updates: List[Int]
)(beforeLookup: Map[Int, Set[Int]], notBeforeLookup: Map[Int, Set[Int]]): Option[Int] = {
  if (isValid(updates)(beforeLookup, notBeforeLookup)) Some(updates(updates.length / 2)) else None
}

def isValid(updates: List[Int])(beforeLookup: Map[Int, Set[Int]], notBeforeLookup: Map[Int, Set[Int]]): Boolean = {
  val (_, isValid) = foldWhile(updates, true) { case (it :: rest, true) =>
    (rest, currentIsValid(it, rest)(beforeLookup, notBeforeLookup))
  }
  isValid
}

def currentIsValid(
    it: Int,
    rest: List[Int]
)(beforeLookup: Map[Int, Set[Int]], notBeforeLookup: Map[Int, Set[Int]]): Boolean = {
  rest.forall { update =>
    beforeLookup.get(it).forall(_.contains(update)) &&
    !notBeforeLookup.get(it).exists(_.contains(update))
  }
}

def invalidUpdate(
    updates: List[Int]
)(beforeLookup: Map[Int, Set[Int]], notBeforeLookup: Map[Int, Set[Int]]): Option[Int] = {
  if (isValid(updates)(beforeLookup, notBeforeLookup)) {
    None
  } else {
    val (_, result) = foldWhile(updates, List.empty[Int]) { case (it :: rest, result) =>
      if (currentIsValid(it, rest)(beforeLookup, notBeforeLookup)) {
        (rest, result :+ it)
      } else {
        (rest :+ it, result)
      }
    }
    Some(result(result.length / 2))
  }
}
