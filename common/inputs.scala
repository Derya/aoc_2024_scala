//> using toolkit latest

def loadInputWeb(
  year: Int,
  day: Int
): String = {
  import sttp.client4.quick.*

  val dayProblemFolderName: String = s"day$day"

  if (!os.exists(os.pwd / dayProblemFolderName))
    sys.error(s"The folder for this day, $dayProblemFolderName, does not exist!")

  val SESSION_PATH = os.pwd / "aoc_session"
  val SAVED_INPUT_PATH = os.pwd / dayProblemFolderName / "input"

  if (os.exists(SAVED_INPUT_PATH)) {
    println("Using saved input file...")
    os.read(SAVED_INPUT_PATH)
  } else {
    println("Fetching input file...")
    val responseBody = quickRequest
      .get(uri"https://adventofcode.com/$year/day/$day/input")
      .header("cookie", "session=%s".format(os.read(SESSION_PATH).trim))
      .send()
      .body
      .trim

    os.write.over(target = SAVED_INPUT_PATH, data = responseBody)

    responseBody
  }
}
