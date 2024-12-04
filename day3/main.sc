import input.input
import scala.util.matching.Regex

def getAnswer1 = {
  val pattern: Regex = "mul\\((\\d{1,3}),(\\d{1,3})\\)".r

  pattern
    .findAllMatchIn(input)
    .foldLeft(0)((acc, x) => acc + x.group(1).toInt * x.group(2).toInt)
}

println(getAnswer1)

def getAnswer2 = {
  val pattern: Regex = "((mul\\((\\d{1,3}),(\\d{1,3})\\))|(do\\(\\))|(don't\\(\\)))".r

  var dooooooooo = true
  var ret = 0
  for (x <- pattern.findAllMatchIn(input)) {
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

println(getAnswer2)
