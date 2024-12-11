//> using file input.scala
//> using file ../common/util.scala

import scala.util.matching.Regex

def getAnswer1 = {
  val pattern: Regex = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r

  pattern
    .findAllMatchIn(buildInput())
    .foldLeft(0)((acc, x) => acc + x.group(1).toInt * x.group(2).toInt)
}

def getAnswer2 = {
  val pattern: Regex = "((mul\\((\\d{1,3}),(\\d{1,3})\\))|(do\\(\\))|(don't\\(\\)))".r

  var dooooooooo = true
  var ret = 0
  for (x <- pattern.findAllMatchIn(buildInput())) {
    if (x.group(3) != null) {
      if (dooooooooo) {
        ret = ret + Integer.valueOf(x.group(3)) * Integer.valueOf(x.group(4))
      }
    } else {
      if (x.group(5) != null) {
        dooooooooo = true
      } else {
        dooooooooo = false
      }
    }
  }

  ret
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
