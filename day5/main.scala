//> using file input.scala
//> using file ../common/util.scala

implicit def GimmeListOpsOnEverything[A](x: IterableOnce[A]): List[A] = x.iterator.toList

val stuffThatComesAfterMe: Map[Int, Set[Int]] =
  buildRules().foldLeft(Map.empty)((acc, rule) =>
    acc + (rule.before -> (acc.getOrElse(rule.before, Set.empty) + rule.after))
  )

val stuffThatComesBeforeMe: Map[Int, Set[Int]] =
  buildRules().foldLeft(Map.empty)((acc, rule) =>
    acc + (rule.after -> (acc.getOrElse(rule.after, Set.empty) + rule.before))
  )

import scala.util.matching.Regex

def reportObeysRules(update: List[Int]): Boolean = {

  val pairedWithPriorElements1: List[(Int, List[Int])] = update.zip(update.inits.reverse) // TODO - utils!
  val pairedWithPriorElements2: List[(Int, List[Int])] = update.zip(update.scanLeft(List[Int]())((b, a) => a :: b).map(_.reverse))

  val anyRuleBroken = update
    .zip(update.inits.reverse)
    .exists((page, previousPages) =>
      previousPages.exists(previousPage => stuffThatComesAfterMe(page).contains(previousPage))
    )

  !anyRuleBroken
}

def getAnswer1 = {
  buildUpdates().map(update => {
    if reportObeysRules(update) then update(update.length / 2) else 0
  }).sum
}

def getAnswer2 = {
  buildUpdates().map(update => {
    val ruleBroken = !reportObeysRules(update)

    if (!ruleBroken) {
      0
    } else {
      import scala.collection.mutable._
      val ordered = ListBuffer.empty[Int]

      for (x <- update) {
        val idx = ordered.lastIndexWhere(orderedElement => stuffThatComesBeforeMe(x).contains(orderedElement))
        ordered.insert(idx + 1, x)
      }

      ordered(ordered.length / 2)
    }
  }).sum
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
