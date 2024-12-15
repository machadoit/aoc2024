import Util.*

@main def day02(): Unit = {

  val input = read("resources/day02")(_.toInt)

  // Part 1

  val result1 = input.map(safeReport).count(identity)

  println(result1)

  // Part 2

  val result2 = input
    .map { report =>
      safeReport(report) || recoverReport(report)
    }
    .count(identity)

  println(result2)
}

enum State:
  case Start, Inc, Dec, Invalid

def safeReport(fullReport: Seq[Int]): Boolean = {
  val (_, finalState) = foldWhile((fullReport.toList, State.Start)) {
    case (current :: rest, state) if state != State.Invalid =>
      (rest, computeState(current, rest, state))
  }

  finalState != State.Invalid
}

def computeState(current: Int, rest: List[Int], state: State): State = {
  rest match {
    case next :: rest =>
      val diff = Math.abs(current - next)
      if (diff >= 1 && diff <= 3) {
        if (next > current && (state == State.Inc || state == State.Start)) {
          State.Inc
        } else if (next < current && (state == State.Dec || state == State.Start)) {
          State.Dec
        } else State.Invalid
      } else {
        State.Invalid
      }
    case _ =>
      state
  }
}

def recoverReport(fullReport: Seq[Int]): Boolean = {
  subReports(fullReport).foldLWhile(false) {
    case (state, report) if !state =>
      safeReport(report)
  }
}

def subReports(lst: Seq[Int]): Seq[Seq[Int]] = {
  val baseListWithIndexes = lst.zipWithIndex

  lst.indices.map(idxToRemove =>
    baseListWithIndexes
      .filterNot((_, idx) => idx == idxToRemove)
      .map((it, _) => it)
  )
}
