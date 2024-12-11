//> using file input.scala
//> using file ../common/util.scala

def getAnswer1 = {
  val matrix = Matrix2D.fromString(getInput()).map(_.asDigit)

  def findReachablePeaks(start: Coord, currentHeight: Int): Set[Coord] = {
    start
      .adjacents
      .flatMap(neighbour => matrix.coordAt(neighbour) match {
        case Some(neighbourHeight) if currentHeight == 8 && neighbourHeight == 9 => Set(neighbour)
        case Some(neighbourHeight) if neighbourHeight == currentHeight + 1 => findReachablePeaks(neighbour, neighbourHeight)
        case _ => Set.empty
      })
      .toSet
  }

  matrix
    .indexedValues
    .filter((_, height) => height == 0)
    .map((trailheadCoordinate, _) => {
      val reachablePeaks = findReachablePeaks(start = trailheadCoordinate, currentHeight = 0)
      reachablePeaks.size
    })
    .sum
}

def getAnswer2 = {
  val matrix = Matrix2D.fromString(getInput()).map(_.toString.toInt)

  def findTrails(start: Coord, currentHeight: Int, pathSoFar: List[Coord]): List[List[Coord]] = {
    start
      .adjacents
      .flatMap(neighbour => matrix.coordAt(neighbour) match {
        case Some(neighbourHeight) if currentHeight == 8 && neighbourHeight == 9 =>
          List(pathSoFar :+ neighbour)
        case Some(neighbourHeight) if neighbourHeight == currentHeight + 1 =>
          findTrails(neighbour, neighbourHeight, pathSoFar :+ neighbour).map(subsequentPath => pathSoFar ::: subsequentPath)
        case _ =>
          List.empty
      })
  }

  matrix
    .indexedValues
    .filter((_, height) => height == 0)
    .map((trailheadCoordinate, _) => {
      val allTrails = findTrails(start = trailheadCoordinate, currentHeight = 0, pathSoFar = List(trailheadCoordinate))
      allTrails.size
    })
    .sum
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
