//> using file input.scala
//> using file ../common/util.scala

import collection.mutable.*
import scala.collection.mutable

def findChar(input: Seq[Seq[Char]], char: Char): Coord = {
  var guard: Option[Coord] = None
  for (y <- input.indices; x <- input(y).indices) {
    if (input(y)(x) == char) {
      guard = Some(Coord(x, y))
    }
  }
  guard.get
}

val guardInitialLocation = findChar(generateInput(), '^')

def mapWithoutGuard(input: Seq[Seq[Char]]): Seq[Seq[Char]] =
  input.map(_.map(tile => if (tile == '^') '.' else tile))

def getAnswer1: Int = {
  val input = mapWithoutGuard(generateInput())

  var left = false
  var ret = 0
  var guard = guardInitialLocation
  var facing = 0

  def getCoord(c: Coord): Char = {
    if (c.x < 0 || c.x >= input(0).length || c.y < 0 || c.y >= input.length) {
      'A'
    } else {
      input(c.y)(c.x)
    }
  }

  def setCoord(c: Coord, v: Char): Unit = {
    input(c.y)(c.x) = v
  }

  def getForward(): Coord = {
    facing match {
      case 0 =>
        Coord(guard.x, guard.y - 1)
      case 1 =>
        Coord(guard.x + 1, guard.y)
      case 2 =>
        Coord(guard.x, guard.y + 1)
      case 3 =>
        Coord(guard.x - 1, guard.y)
    }
  }

  def rotate(): Unit = {
    facing = facing + 1
    if (facing > 3) {
      facing = 0
    }
  }

  while (!left) {
    // mark where we were
    if (getCoord(guard) != 'X') {
      ret += 1
    }
    setCoord(guard, 'X')

    // move forward
    guard = getForward()

    if (guard.x < 0 || guard.x >= input(0).length || guard.y < 0 || guard.y >= input.length) {
      // we've left
      left = true
    } else {
      // rotate as much as needed
      while (getCoord(getForward()) == '#') {
        rotate()
      }
    }
  }

  ret
}

def getAnswer2 = {

  def infiniteLoops(input: Seq[Seq[Char]]): Boolean = {
    val directions: Map[Coord, Set[Int]] = Map.empty

    var infiniteLoop = false
    var left = false
    var guard = guardInitialLocation
    var facing = 0

    def getCoord(c: Coord): Char = {
      if (c.x < 0 || c.x >= input(0).length || c.y < 0 || c.y >= input.length) {
        'A'
      } else {
        input(c.y)(c.x)
      }
    }

    def setCoord(c: Coord, v: Char): Unit = {
      input(c.y)(c.x) = v
    }

    def getForward(): Coord = {
      facing match {
        case 0 =>
          Coord(guard.x, guard.y - 1)
        case 1 =>
          Coord(guard.x + 1, guard.y)
        case 2 =>
          Coord(guard.x, guard.y + 1)
        case 3 =>
          Coord(guard.x - 1, guard.y)
      }
    }

    def rotate(): Unit = {
      facing = facing + 1
      if (facing > 3) {
        facing = 0
      }
    }

    while (!left && !infiniteLoop) {
      val directionsForThisCoord = directions.getOrElse(guard, Set.empty)

      // check if we've been here before
      if (directionsForThisCoord.contains(facing)) {
        infiniteLoop = true
      }

      // add this to the history for this spot
      directionsForThisCoord += facing
      directions.put(guard, directionsForThisCoord)

      // move forward
      guard = getForward()

      if (guard.x < 0 || guard.x >= input(0).length || guard.y < 0 || guard.y >= input.length) {
        // we've left
        left = true
      } else {
        // rotate as much as needed
        while (getCoord(getForward()) == '#') {
          rotate()
        }
      }
    }

    infiniteLoop
  }

  val initialMap = mapWithoutGuard(generateInput())

  var places = 0
  for (y <- initialMap.indices; x <- initialMap(y).indices) {
    if (x == guardInitialLocation.x && y == guardInitialLocation.y) {
      // cant drop a box of light bulbs on the guard
    } else {
      val provisionalInput = mapWithoutGuard(generateInput())
      provisionalInput(x)(y) = '#'
      if (infiniteLoops(provisionalInput)) {
        places += 1
      }
    }
  }

  places

}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
