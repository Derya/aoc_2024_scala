import input.input

val reports: Vector[Vector[Int]] = input.map(report => report.split(" ").map(_.toInt).toVector)

def reportIsSafe(report: Vector[Int]): Boolean = {

  def reportIsAscendingAndSafe(report: Vector[Int]): Boolean = {
    val violationExists = report.sliding(2).exists(window => {
      val left = window(0)
      val right = window(1)

      (left >= right) || (right - left > 3)
    })

    !violationExists
  }

  report.length match
    case 0 | 1 =>
      true
    case _ if report(0) == report(1) =>
      false
    case _ if report(0) < report(1) =>
      reportIsAscendingAndSafe(report)
    case _ if report(0) > report(1) =>
      reportIsAscendingAndSafe(report.reverse)
}

def getAnswer1 = {
  reports.count(r => reportIsSafe(r))
}

println(getAnswer1)

def getAnswer2 = {
  reports.count(report => {
    report.indices.exists(index => reportIsSafe(report.patch(index, Nil, 1)))
  })
}

println(getAnswer2)
