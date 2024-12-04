import input.input
import scala.util.matching.Regex

val grid: Array[Array[Char]] = input.split("\n").map(_.toCharArray)

val pattern1: Regex = "XMAS".r
val pattern2: Regex = "SAMX".r

def normalMatches(): Int = {
  var ret = 0

  for (x <- grid.indices) {
    ret = ret + pattern1.findAllMatchIn(grid(x).mkString).count(x => true)
    ret = ret + pattern2.findAllMatchIn(grid(x).mkString).count(x => true)
  }

  for (y <- grid(0).indices) {
    val column = grid.indices.map(grid(_)(y)).mkString
    ret = ret + pattern1.findAllMatchIn(column).count(x => true)
    ret = ret + pattern2.findAllMatchIn(column).count(x => true)
  }

  ret
}

def diagMatches(): Int = {
  var ret = 0

  for (d <- -(grid.length - 1) until grid(0).length) {
    val diagonal = for {
      x <- grid.indices
      y = x + d
      if y >= 0 && y < grid(x).length
    } yield grid(x)(y)

    ret = ret + pattern1.findAllMatchIn(diagonal.mkString).count(x => true)
    ret = ret + pattern2.findAllMatchIn(diagonal.mkString).count(x => true)
  }

  for (d <- 0 until (grid.length + grid(0).length - 1)) {
    val diagonal = for {
      x <- grid.indices
      y = d - x
      if y >= 0 && y < grid(x).length
    } yield grid(x)(y)

    ret = ret + pattern1.findAllMatchIn(diagonal.mkString).count(x => true)
    ret = ret + pattern2.findAllMatchIn(diagonal.mkString).count(x => true)
  }

  ret
}

def getAnswer1 = {
  normalMatches() + diagMatches()
}

println(getAnswer1)

def getAnswer2 = {
  def coordOrQ(x: Int, y: Int): Char = {
    if (x >= 0 && x < grid.length && y >= 0 && y < grid(x).length) {
      grid(x)(y)
    } else {
      'Q'
    }
  }

  var ret = 0

  for (x <- grid.indices; y <- grid(x).indices) {
    if (grid(x)(y) == 'A') {
      val mas1 = (coordOrQ(x - 1, y - 1), coordOrQ(x + 1, y + 1)) match {
        case ('M', 'S') => true
        case ('S', 'M') => true
        case _ => false
      }

      val mas2 = (coordOrQ(x - 1, y + 1), coordOrQ(x + 1, y - 1)) match {
        case ('M', 'S') => true
        case ('S', 'M') => true
        case _ => false
      }

      ret = ret + (if (mas1 && mas2) 1 else 0)
    }
  }

  ret
}

println(getAnswer2)
