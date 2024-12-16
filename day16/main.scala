//> using file ../common/util.scala
//> using file ../common/inputs.scala

import scala.collection.mutable
import Direction.*

def parseInput() =
  loadInputAsMatrix(2024, 16)

case class State(
  location: Location,
  gCost: Long,
  path: Path
)

case class Location(coord: Coord, direction: Direction)

case class Path(locations: List[Location])

def fullSearch(
  map: Matrix2D[Char],
  start: Location,
  goal: Coord
): List[(Path, Long)] = {
  val statesToSearch = mutable.Queue[State](State(start, 0, Path(List.empty)))
  val bestCostToLocation = mutable.Map[Location, Long]().withDefaultValue(Long.MaxValue)
  val bestPaths = mutable.ListBuffer[(Path, Long)]()

  bestCostToLocation(start) = 0

  while (statesToSearch.nonEmpty) {
    val current = statesToSearch.dequeue()
    val coord = current.location.coord
    val direction = current.location.direction

    if (coord == goal) {
      if (bestPaths.isEmpty) {
        bestPaths += ((current.path, current.gCost))
      } else {
        val (path, gCost) = bestPaths.head

        if (current.gCost < gCost) {
          bestPaths.clear()
          bestPaths += ((current.path, current.gCost))
        } else if (current.gCost == gCost) {
          bestPaths += ((current.path, current.gCost))
        } else if (current.gCost > gCost) {
          // no op
        }
      }
    }

    val neighbors: List[(Location, Long)] = {
      val unchecked = List(
        (Location(coord.moveDirection(direction), direction), 1L),
        (Location(coord, direction.cw), 1000L),
        (Location(coord, direction.ccw), 1000L)
      )

      unchecked.filter((loc, _) => map.coordAt(loc.coord).getOrElse('Q') == '.')
    }

    for ((neighbor, cost) <- neighbors) {
      val tentativeGScore = bestCostToLocation(current.location) + cost

      if (tentativeGScore <= bestCostToLocation(neighbor)) {
        bestCostToLocation(neighbor) = tentativeGScore
        statesToSearch.enqueue(State(neighbor, tentativeGScore, Path(current.path.locations :+ neighbor)))
      }
    }
  }

  bestPaths.toList
}

def getAnswer1 = {
  val input: Matrix2D[Char] = parseInput()
  val end = input.findAny(_ == 'E').get
  val start = input.findAny(_ == 'S').get

  val maze = input
    .updated(end, '.')
    .updated(start, '.')

  val allPaths = fullSearch(
    map = maze,
    start = Location(start, East),
    goal = end,
//    heuristic = location => (end.x - location.coord.x).abs + (end.y - location.coord.y).abs
  )

  val (path, cost) = allPaths.head

  cost
}


def getAnswer2 = {
  val input: Matrix2D[Char] = parseInput()
  val end = input.findAny(_ == 'E').get
  val start = input.findAny(_ == 'S').get

  val maze = input
    .updated(end, '.')
    .updated(start, '.')

  val allPaths: List[(Path, Long)] = fullSearch(
    map = maze,
    start = Location(start, East),
    goal = end,
//    heuristic = location => (end.x - location.coord.x).abs + (end.y - location.coord.y).abs
  )

  allPaths
    .map((path, cost) => path)
    .flatMap(path => path.locations)
    .map(location => location.coord)
    .distinct
    .size + 1 // paths don't count end tile!!! >:(
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
