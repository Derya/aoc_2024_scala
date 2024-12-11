//> using file input.scala

def getAnswer1 = {
  buildList1().sorted.zip(buildList2().sorted).foldLeft(0L) {
    case (acc, (left, right)) => acc + (left - right).abs
  }
}

def getAnswer2 = {
  val instanceCount: Map[Long, Int] = buildList2().groupBy(x => x).map((num, instances) => (num, instances.size))

  buildList1().foldLeft(0L)((acc, left) => acc + (left * instanceCount.getOrElse(left, 0)))
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
