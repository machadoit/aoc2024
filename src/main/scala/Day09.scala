import Util.{foldWhile, read}

@main def day09(): Unit = {
  val input = read("resources/day09", delim = "")(_.toInt).head.toArray

  // Part 1

  val filesystem1 = buildFilesystem(input)
  defrag1(filesystem1)

  val result1 = checksum(filesystem1)

  println(result1)

  // Part 2
  val filesystem2 = buildFilesystem(input)
  defrag2(filesystem2)

  val result2 = checksum(filesystem2)

  println(result2)
}

def buildFilesystem(input: Array[Int]): Array[Int] = {
  input.zipWithIndex.flatMap { (value, idx) =>
    if (idx % 2 == 0) {
      Array.fill(value)(idx / 2)
    } else {
      Array.fill(value)(-1)
    }
  }
}

def defrag1(filesystem: Array[Int]) = {

  foldWhile(
    filesystem.zipWithIndex.iterator.filter(_._1 == -1),
    filesystem.zipWithIndex.reverseIterator.filter(_._1 >= 0)
  ) {
    case (emptyIt, fileIt) if emptyIt.hasNext && fileIt.hasNext =>
      val (empty, emptyIdx) = emptyIt.next()
      val (file, fileIdx) = fileIt.next()

      if (emptyIdx >= fileIdx) {
        (Iterator.empty[(Int, Int)], Iterator.empty[(Int, Int)])
      } else {
        filesystem.update(emptyIdx, file)
        filesystem.update(fileIdx, -1)
        (emptyIt, fileIt)
      }
  }
}

def defrag2(filesystem: Array[Int]) = {
  given Array[Int] = filesystem
  foldWhile(nextFile(filesystem.length - 1)) {
    case (fileIdx, fileSize) if fileIdx - fileSize > 0 =>
      nextEmptyBefore(fileSize, fileIdx).foreach { emptyIdx =>
        Array.copy(filesystem, fileIdx, filesystem, emptyIdx, fileSize)
        Array.copy(Array.fill(fileSize)(-1), 0, filesystem, fileIdx, fileSize)
      }
      nextFile(fileIdx - 1)
  }
}

def nextEmptyBefore(fileSize: Int, before: Int)(using filesystem: Array[Int]): Option[Int] = {
  val it = filesystem.iterator.zipWithIndex
  var block = Array.empty[(Int, Int)]

  while (block.length < fileSize && it.hasNext) {
    block = it.dropWhile(_._1 != -1).takeWhile(_._1 == -1).toArray
  }

  if (fileSize <= block.length) { block.headOption.map(_._2).filter(_ < before) }
  else None
}

def nextFile(idxFrom: Int)(using filesystem: Array[Int]): (Int, Int) = {
  val idx = foldWhile(idxFrom) { case idx if filesystem(idx) < 0 => idx - 1 }
  val symbol = filesystem(idx)

  foldWhile(idx, 1) { case (idx, size) if filesystem.lift(idx - 1).contains(symbol) => (idx - 1, size + 1) }
}

def checksum(filesystem: Array[Int]): Long = {
  filesystem.zipWithIndex.filter(_._1 > 0).foldLeft(0L) { case (total, (file, idx)) =>
    total + (file * idx)
  }
}
