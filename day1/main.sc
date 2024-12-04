import input.{list1, list2}

def getAnswer1 = {
  list1.sorted.zip(list2.sorted).foldLeft(0L) {
    case (acc, (left, right)) => acc + Math.abs(left - right)
  }
}

println(getAnswer1)

def getAnswer2 = {
  val instanceCount: Map[Long, Int] = list2.groupBy(x => x).map((num, instances) => (num, instances.size))

  list1.foldLeft(0L)((acc, left) => acc + (left * instanceCount.getOrElse(left, 0)))
}

println(getAnswer2)


