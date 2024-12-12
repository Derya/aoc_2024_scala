//> using file ../common/util.scala
//> using file ../common/inputs.scala

def parseInput(): List[BigInt] =
  loadInputWeb(2024, 11)
    .split(" ")
    .map(BigInt(_))
    .toList

def getAnswer1 = {
  def iter(stones: List[BigInt]): List[BigInt] =
    stones.flatMap(stone => {
      if (stone == 0) {
        List(1)
      } else {
        val stoneAsString = stone.toString(10)

        if (stoneAsString.length % 2 == 0) {
          val (leftDigits, rightDigits) = stoneAsString.splitAt(stoneAsString.length / 2)

          List(BigInt(leftDigits), BigInt(rightDigits))
        } else {
          List(stone * 2024)
        }
      }
    })

  (1 to 25)
    .foldLeft(parseInput())((currentStones, blink) => iter(currentStones))
    .length
}


def getAnswer2 = {
  val countCache = Cacher[(BigInt, Int), BigInt]

  def count(stone: BigInt, blinksRemaining: Int): BigInt = {
    if (blinksRemaining == 0) {
      1
    } else {
      countCache.getOrElseUpdate(
        (stone, blinksRemaining),
        if (stone == 0) {
          count(1, blinksRemaining - 1)
        } else {
          val stoneAsString = stone.toString(10)

          if (stoneAsString.length % 2 == 0) {
            val (leftDigits, rightDigits) = stoneAsString.splitAt(stoneAsString.length / 2)

            count(BigInt(leftDigits), blinksRemaining - 1) + count(BigInt(rightDigits), blinksRemaining - 1)
          } else {
            count(stone * 2024, blinksRemaining - 1)
          }
        }
      )
    }
  }

  parseInput().map(stone => count(stone, 75)).sum
}

@main
def main(): Unit = {
  println(getAnswer1)
  println(getAnswer2)
}
