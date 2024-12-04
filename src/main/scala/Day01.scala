import Util.read

@main def day01(): Unit = {

  val input = read("resources/day01")(_.toInt)

  // Part 1

  val (leftL, rightL) = locations(input)
  val distances = leftL.sorted.zip(rightL.sorted).map((l, r) => Math.abs(l - r))
  val result1 = distances.sum

  println(result1)

  // Part 2

  val rLookup = occurrenceLookup(rightL)

  val result2 = leftL.foldLeft(0) { case (total, id) =>
    total + (id * rLookup.getOrElse(id, 0))
  }

  println(result2)
}

def locations(input: List[List[Int]]): (Seq[Int], Seq[Int]) = {
  input.foldLeft((List.empty[Int], List.empty[Int])) { case ((leftL, rightL), left :: right :: Nil) =>
    (left +: leftL, right +: rightL)
  }
}

def occurrenceLookup(lst: Seq[Int]): Map[Int, Int] = {
  lst.groupBy(identity).map { (key, values) =>
    (key, values.length)
  }
}
