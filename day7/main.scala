//> using file input.scala
//> using file ../common/util.scala

def getAnswer1 = {
  def can(target: Long, left: Long, right: List[Long]): Boolean = {
    right match {
      case head :: Nil =>
        (target == left + head) || (target == left * head)
      case head :: tail =>
        can(target, left + head, tail) || can(target, left * head, tail)
      case _ =>
        sys.error("empty list")
    }
  }

  def canOperatize(in: In): Boolean = {
    in.operatees.toList match
      case head :: tail => can(in.output, head, tail)
      case _ => sys.error("list less than size 2")
  }

  generateInput()
    .collect({ case x if canOperatize(x) => BigInt(x.output) })
    .sum
}

def getAnswer2 = {
  import math.BigInt.long2bigInt

  def can(target: BigInt, left: BigInt, right: List[Long]): Boolean = {
    right match {
      case head :: Nil =>
        (target == left + head) || (target == left * head) || (target == BigInt(s"${left.toString}${head.toString}"))
      case head :: tail =>
        can(target, left + head, tail) || can(target, left * head, tail) || can(target, BigInt(s"${left.toString}${head.toString}"), tail)
      case _ =>
        sys.error("empty list")
    }
  }

  def canOperatize(in: In): Boolean = {
    in.operatees.toList match
      case head :: tail => can(BigInt(in.output), BigInt(head), tail)
      case _ => sys.error("list less than size 2")
  }

  generateInput()
    .collect({ case x if canOperatize(x) => BigInt(x.output) })
    .sum
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
