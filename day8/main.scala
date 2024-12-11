//> using file input.scala
//> using file ../common/util.scala

def antennaeByType(matrix: Matrix2D[Char]): Map[Char, Set[Coord]] =
  matrix.indexedValues.foldLeft(Map.empty) { case (acc, (coord, char)) =>
    if char == '.' then
      acc
    else
      acc + (char -> (acc.getOrElse(char, Set.empty) + coord))
  }

def getAnswer1 = {
  val input = Matrix2D.fromString(generateInput())

  val antinodes = antennaeByType(input).flatMap((char, antennae) => {
    // find all antinodes for this char
    val thisCharAntinodes = antennae
      .cross(antennae)
      .filter(_ != _)
      .flatMap((antenna1, antenna2) => {
        val delta = antenna2 - antenna1
        val antinode1 = antenna2 + delta
        val antinode2 = antenna1 - delta
        List(antinode1, antinode2)
      })

    thisCharAntinodes
  })

  antinodes
    .toSet
    .count(antinode => input.coordAt(antinode).isDefined)
}

def getAnswer2 = {
  val input = Matrix2D.fromString(generateInput())

  val antinodes = antennaeByType(input).flatMap((char, antennae) => {
    // find all antinodes for this char
    val thisCharAntinodes = antennae
      .cross(antennae)
      .filter(_ != _)
      .flatMap((antenna1, antenna2) => {
        import scala.collection.mutable.ListBuffer
        
        val slope = (antenna2 - antenna1).shrinkRetainSlope()

        val antinodes = ListBuffer.empty[Coord]

        // antenna 1, going positive direction
        {
          var i = 0
          var coord = antenna1 + (slope * i)
          while (input.coordAt(coord).isDefined) {
            antinodes += coord
            i += 1
            coord = antenna1 + (slope * i)
          }
        }

        // antenna 1, going negative direction
        {
          var i = -1
          var coord = antenna1 + (slope * i)
          while (input.coordAt(coord).isDefined) {
            antinodes += coord
            i -= 1
            coord = antenna1 + (slope * i)
          }
        }

        // antenna 2, going positive direction
        {
          var i = 0
          var coord = antenna2 + (slope * i)
          while (input.coordAt(coord).isDefined) {
            antinodes += coord
            i += 1
            coord = antenna2 + (slope * i)
          }
        }

        // antenna 2, going negative direction
        {
          var i = -1
          var coord = antenna2 + (slope * i)
          while (input.coordAt(coord).isDefined) {
            antinodes += coord
            i -= 1
            coord = antenna2 + (slope * i)
          }
        }

        antinodes
      })

    thisCharAntinodes
  })

  antinodes
    .toSet
    .count(_ => true)
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
