//> using file input.scala
//> using file ../common/util.scala

sealed trait DiskRange {
  def size: Int
}

case class EmptyRange(size: Int) extends DiskRange

case class FileRange(id: Int, size: Int) extends DiskRange

def parseInput(): Array[DiskRange] = {
  input
    .toCharArray
    .map(_.asDigit)
    .mapWithIndex((size, idx) => {
      if (idx % 2 == 0) {
        FileRange(idx / 2, size)
      } else {
        EmptyRange(size)
      }
    })
}

sealed trait DiskBlock

case object Empty extends DiskBlock

case class File(id: Int) extends DiskBlock

def toBlocks(ranges: Array[DiskRange]): Array[DiskBlock] =
  ranges.flatMap {
    case EmptyRange(size) => List.fill(size)(Empty)
    case FileRange(id, size) => List.fill(size)(File(id))
  }

def getAnswer1 = {
  val blocks = toBlocks(parseInput())

  for (idx <- blocks.indices.reverse) {
    blocks(idx) match
      case File(id) =>
        val targetIdx = blocks.indices.find(j => blocks(j) == Empty).get
        if (targetIdx < idx) {
          blocks(targetIdx) = File(id)
          blocks(idx) = Empty
        }
        ()
      case _ =>
        ()
  }

  blocks
    .mapWithIndex((block, idx) => {
      block match {
        case File(id) => BigInt(id * idx)
        case Empty => BigInt(0)
      }
    })
    .sum
}

def getAnswer2 = {
  var disk: Array[DiskRange] = parseInput()

  val highestId = disk
    .collect({
      case FileRange(id: Int, _) => id
    })
    .max

  (0 to highestId).reverse.foreach(idToMove => {
    val (fileToMove: DiskRange, idxOfFile: Int) = disk.findWithIdx({
      case FileRange(id, _) => id == idToMove
      case _ => false
    }).get

    disk.findWithIdx({
      case EmptyRange(size) => size >= fileToMove.size
      case _ => false
    }).map((spaceToMoveTo, idxOfSpace) => {

      if (idxOfSpace < idxOfFile) {
        val spaceLeft = spaceToMoveTo.size - fileToMove.size

        if (spaceLeft == 0) {
          disk(idxOfFile) = EmptyRange(fileToMove.size)
          disk(idxOfSpace) = FileRange(idToMove, fileToMove.size)
        } else {
          disk(idxOfFile) = EmptyRange(fileToMove.size)
          disk = disk.patch(
            idxOfSpace,
            List(
              fileToMove,
              EmptyRange(spaceLeft)
            ),
            1
          )
        }
      }
    })
  })

  toBlocks(disk)
    .mapWithIndex((block, idx) => {
      block match {
        case File(id) => BigInt(id * idx)
        case Empty => BigInt(0)
      }
    })
    .sum
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
