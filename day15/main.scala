//> using file ../common/util.scala
//> using file ../common/inputs.scala

import Direction.*

import scala.annotation.tailrec

def warehouseInput = loadInputAs2Parter(2024, 15)._1

def movementsInput = loadInputAs2Parter(2024, 15)._2

def warehouseInputExample = """##########
                       |#..O..O.O#
                       |#......O.#
                       |#.OO..O.O#
                       |#..O@..O.#
                       |#O#..O...#
                       |#O..O..O.#
                       |#.OO.O.OO#
                       |#....O...#
                       |##########""".stripMargin

def movementsInputExample = """<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
                       |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
                       |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
                       |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
                       |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
                       |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
                       |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
                       |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
                       |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
                       |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^""".stripMargin

def moves: Seq[Direction] = movementsInput
  .map(Direction.fromChar)
  .collect { case Some(direction) => direction }

def getAnswer1 = {
  def warehouse = Matrix2D.fromString(warehouseInput)

  def canPushBox(state: Matrix2D[Char], direction: Direction, boxLocation: Coord): Option[Coord] = {
    val moveBoxTo: Coord = boxLocation.moveDirection(direction)

    state.coordAt(moveBoxTo).get match {
      case '#' => None
      case '.' => Some(moveBoxTo)
      case 'O' => canPushBox(state, direction, moveBoxTo)
      case x => sys.error("unexpected tile " + x + " at location " + moveBoxTo)
    }
  }

  val finalState = moves.foldLeft(warehouse)((state, direction) => {
    val robot = state.findAny(_ == '@').get
    val moveTo = robot.moveDirection(direction)

    state.coordAt(moveTo).get match {
      case '#' =>
        state

      case '.' =>
        state
          .updated(robot, '.')
          .updated(moveTo, '@')

      case 'O' =>
        canPushBox(state, direction, moveTo)
          .map(finalLocation =>
            state
              .updated(finalLocation, 'O')
              .updated(robot, '.')
              .updated(moveTo, '@')
          )
          .getOrElse(state)

      case x =>
        sys.error("unexpected tile " + x + " at location " + moveTo)
    }
  })

  finalState
    .indexedValues
    .map((coord, tile) => if (tile == 'O') (100 * coord.x) + coord.y else 0)
    .sum
}

def getAnswer2 = {
  val modifiedWarehouseLines = warehouseInput
    .linesIterator
    .map(line => {
      val newLine = line
        .toCharArray
        .toList
        .flatMap(char =>
          char match {
            case '#' => List('#', '#')
            case '.' => List('.', '.')
            case 'O' => List('[', ']')
            case '@' => List('@', '.')
            case x => sys.error("unexpected tile " + x)
          }
        )

      newLine.mkString
    })
    .toList

  def buildNewWarehouse = Matrix2D.fromLines(modifiedWarehouseLines)

  case class Box(left: Coord, right: Coord)

  def findBox(state: Matrix2D[Char], at: Coord): Option[Box] =
    state.coordAt(at).get match {
      case '[' => Some(Box(at, at.moveDirection(East)))
      case ']' => Some(Box(at.moveDirection(West), at))
      case _ => None
    }

  def findDownstreamBoxes(state: Matrix2D[Char], direction: Direction, box: Box): List[Box] =
    direction match {
      case North | South =>
        val left = findBox(state, box.left.moveDirection(direction))
          .map(findDownstreamBoxes(state, direction, _))
          .getOrElse(Nil)

        val right = findBox(state, box.right.moveDirection(direction))
          .map(findDownstreamBoxes(state, direction, _))
          .getOrElse(Nil)

        box :: left ++ right

      case East =>
        val right = findBox(state, box.right.moveDirection(direction))
          .map(findDownstreamBoxes(state, direction, _))
          .getOrElse(Nil)

        box :: right

      case West =>
        val left = findBox(state, box.left.moveDirection(direction))
          .map(findDownstreamBoxes(state, direction, _))
          .getOrElse(Nil)

        box :: left
    }

  def tryMoveBox(box: Box, state: Matrix2D[Char], direction: Direction, robot: Coord, moveTo: Coord) = {
    val allDownstreamBoxes = findDownstreamBoxes(state, direction, box).distinct

    val allCanMove = allDownstreamBoxes.forall(box => {
      val leftMoveTo = state.coordAt(box.left.moveDirection(direction)).get
      val rightMoveTo = state.coordAt(box.right.moveDirection(direction)).get

      val leftIsClear = leftMoveTo == '.' || leftMoveTo == '[' || leftMoveTo == ']'
      val rightIsClear = rightMoveTo == '.' || rightMoveTo == '[' || rightMoveTo == ']'

      leftIsClear && rightIsClear
    })

    if (!allCanMove) {
      println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!CANNOT MOVE " + allDownstreamBoxes.size + " BOXES " + direction + "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      println(state.prettyPrint)

      state
    } else {
      val ret = allDownstreamBoxes
        .sortWith((box1, box2) => box1.left.isDirectionOf(direction, box2.left))
        .foldLeft(state)((newState, x) =>
          newState
            .updated(x.left, '.')
            .updated(x.right, '.')
            .updated(x.left.moveDirection(direction), '[')
            .updated(x.right.moveDirection(direction), ']')
        )
        .updated(robot, '.')
        .updated(moveTo, '@')

      println("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!MOVING " + allDownstreamBoxes.size + " BOXES " + direction + "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      println(state.prettyPrint)
      println(ret.prettyPrint)

      ret
    }
  }

  val finalState = moves.foldLeft(buildNewWarehouse)((state, direction) => {
    val robot = state.findAny(_ == '@').get
    val moveTo = robot.moveDirection(direction)

    state.coordAt(moveTo).get match {
      case '#' =>
        state

      case '.' =>
        state
          .updated(robot, '.')
          .updated(moveTo, '@')

      case '[' =>
        tryMoveBox(
          box = Box(moveTo, moveTo.moveDirection(East)),
          state = state,
          direction = direction,
          robot = robot,
          moveTo = moveTo
        )

      case ']' =>
        tryMoveBox(
          box = Box(moveTo.moveDirection(West), moveTo),
          state = state,
          direction = direction,
          robot = robot,
          moveTo = moveTo
        )

      case x =>
        sys.error("unexpected tile " + x + " at location " + moveTo)
    }
  })

  finalState.indexedValues.map((coord, tile) => {
    if (tile == '[') {
      val thisBoxLeftCoord = coord
      val thisBoxRightCoord = coord.moveDirection(East)
      val distanceToLeft = thisBoxLeftCoord.y
      (100 * coord.x) + distanceToLeft
//      val distanceToRight = 99 - thisBoxRightCoord.y // input is 50 wide, so 100 for enlarged warehouse
//      if (distanceToLeft < distanceToRight) {
//        (100 * coord.x) + distanceToLeft
//      } else {
//        (100 * coord.x) + distanceToRight
//      }
    } else {
      0
    }
  }).sum
}

@main
def main(): Unit = {
//  println(getAnswer1)
  println(getAnswer2)
}
