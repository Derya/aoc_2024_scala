//> using toolkit latest
//> using lib "io.circe::circe-generic:0.14.10"
//> using lib "io.circe::circe-parser:0.14.10"

enum Board(val id: Long) {
  case scala extends Board(712467)
  case c3d extends Board(1513917)
  case lhl extends Board(17376)
}

enum StarType {
  case First
  case Second
}

case class Star(day: Int, part: StarType)

object AocApi {
  case class StarData(star_index: Long, get_star_ts: Long)

  case class Member(
    local_score: Long,
    id: Long,
    stars: Long,
    last_star_ts: Long,
    name: Option[String],
    global_score: Long,
    completion_day_level: Map[Int, Map[Int, StarData]]
  ) {
    val displayName = name.getOrElse("Anonymous")

    val starData: Map[Star, StarData] = completion_day_level.flatMap {
      case (outerKey, innerMap) =>
        innerMap.map {
          case (1, starData) => (Star(outerKey, StarType.First), starData)
          case (2, starData) => (Star(outerKey, StarType.Second), starData)
          case _ => sys.error("unexpected star level")
        }
    }
  }

  case class ApiResp(event: String, members: Map[String, Member])
}

object Ansi {
  val Reset = "\u001b[0m"
  val Red = "\u001b[31m"
  val Green = "\u001b[32m"
  val Yellow = "\u001b[33m"
  val Orange = "\u001b[38;5;202m"
  val Blue = "\u001b[34m"
  val BlueCyan = "\u001b[38;5;39m"
  val Magenta = "\u001b[35m"
  val Cyan = "\u001b[36m"
  val Silver = "\u001b[38;5;145m"
  val Grey = "\u001b[38;5;8m"
  val White = "\u001b[37m"
  val Bold = "\u001b[1m"
  val Underline = "\u001b[4m"
}

/** returns a timestamp in seconds */
def getAdventTime(adventCalendarDay: Int, year: Int): Long = {
  import java.time.{Instant, LocalDate, ZoneId}

  LocalDate.of(year, 12, adventCalendarDay)
    .atStartOfDay(ZoneId.of("America/New_York"))
    .toInstant
    .toEpochMilli / 1000
}

def getBoardData(
  board: Board,
  year: Int
): AocApi.ApiResp = {
  import sttp.client4.Response
  import sttp.client4.quick.*

  import java.time.Instant
  import io.circe.*
  import io.circe.generic.auto.*
  import io.circe.parser.*
  import io.circe.syntax.*

  val SESSION_PATH = os.pwd / "aoc_session"
  val CACHED_JSON_PATH = os.pwd / "cache" / s"aoc_saved_json_${board.id}"
  val CACHED_JSON_DATE_PATH = os.pwd / "cache" / s"aoc_saved_json_date_${board.id}"

  // don't hit API more than once per 15 minutes
  val useCached = os.exists(CACHED_JSON_DATE_PATH) &&
    Instant.ofEpochMilli(os.read(CACHED_JSON_DATE_PATH).trim.toLong).isAfter(Instant.now.minusSeconds(15 * 60))

  val jsonString: String = if (useCached) {
    println("Using cached JSON...")
    os.read(CACHED_JSON_PATH)
  } else {
    println("Fetching fresh JSON...")
    val responseBody = quickRequest
      .get(uri"https://adventofcode.com/$year/leaderboard/private/view/${board.id}.json")
      .header("cookie", "session=%s".format(os.read(SESSION_PATH).trim))
      .send()
      .body

    os.write.over(
      target = CACHED_JSON_PATH,
      data = responseBody,
      createFolders = true
    )
    os.write.over(
      target = CACHED_JSON_DATE_PATH,
      data = Instant.now.toEpochMilli.toString,
      createFolders = true
    )

    responseBody
  }

  decode[AocApi.ApiResp](jsonString).right.get
}

def buildBoardLog(
  board: Board,
  year: Int,
  usersPerDay: Int,
  daysToShow: Int,
  hoursCutoff: Option[Int] = None
): List[String] = {
  import AocApi._

  val allMembers: List[Member] =
    getBoardData(board, year)
      .members
      .map((_, member) => member).toList

  val allDays: List[Int] =
    allMembers
      .flatMap(_.starData.keys)
      .map(_.day)
      .distinct

  allDays
    .sortBy(x => x)
    .reverse
    .take(daysToShow)
    .flatMap(day => {
      import Ansi._

      def formatDuration(totalSeconds: Long): String = {
        val hours = totalSeconds / 3600
        val minutes = (totalSeconds % 3600) / 60
        val seconds = totalSeconds % 60

        f"$hours%02d:$minutes%02d:$seconds%02d"
      }

      def columnForStar(star: Star): List[String] =
        allMembers
          .filter(_.starData.contains(star))
          .sortBy(_.starData(star).get_star_ts)
          .take(usersPerDay)
          .map(user => (user, user.starData(star).get_star_ts - getAdventTime(adventCalendarDay = day, year = year)))
          .filter((_, timeTook) => hoursCutoff.forall(timeTook < 60 * 60 * _))
          .map((member, timeTook) => s"  ${formatDuration(timeTook)} $Orange${member.displayName}")

      val maxSideLen = 42

      val star1 = columnForStar(Star(day, StarType.First))
      val star2 = columnForStar(Star(day, StarType.Second))

      val topUserLines = star1.zipAll(star2, "", "").map((left, right) =>
        if (left.length < maxSideLen) {
          left + " ".repeat(maxSideLen - left.length) + Reset + right + Reset
        } else {
          left.substring(0, maxSideLen) + Reset + right + Reset
        }
      )

      List(
        "",
        s"  $Bold$BlueCyan==== DAY $day ====$Reset",
        "",
        s"  $Silver$Bold*$Reset ${Grey}Part 1                       $Yellow$Bold*$Reset ${Grey}Part 2$Reset"
      ) ++ topUserLines
    })
}

buildBoardLog(
  board = Board.scala,
  year = 2024,
  usersPerDay = 5,
  daysToShow = 5,
  hoursCutoff = None
).foreach(println)

println()
