
board="scala"
numDays=5
numUsers=5
hoursCutoff=-1
year=2024

while [[ $# -gt 0 ]]; do
  case $1 in
    --board)
      board="$2"
      shift 2
      ;;
    --numDays)
      if [[ "$2" =~ ^[0-9]+$ ]]; then
        numDays=$2
      else
        echo "arg 'numDays' must be an integer"
        exit 1
      fi
      shift 2
      ;;
    --numUsers)
      if [[ "$2" =~ ^[0-9]+$ ]]; then
        numUsers=$2
      else
        echo "arg 'numUsers' must be an integer"
        exit 1
      fi
      shift 2
      ;;
    --hoursCutoff)
      if [[ "$2" =~ ^[0-9]+$ ]]; then
        hoursCutoff=$2
      else
        echo "arg 'hoursCutoff' must be an integer."
        exit 1
      fi
      shift 2
      ;;
    --year)
      if [[ "$2" =~ ^[0-9]+$ ]]; then
        year=$2
      else
        echo "arg 'year' must be an integer."
        exit 1
      fi
      shift 2
      ;;
    *)
      echo "Unknown option: $1"
      exit 1
      ;;
  esac
done

if [[ $hoursCutoff == -1 ]]; then
  hoursCutoffArg="None"
else
  hoursCutoffArg="Some($hoursCutoff)"
fi

if [[ $board =~ ^[0-9]+$ ]]; then
  echo "Displaying leaderboard data for private leaderboard with ID $board."
  boardArg="$board"
else
  echo "Displaying leaderboard data for the '$board' private leaderboard, who I sure hope is listed in the enum!"
  boardArg="AdventPrinter.Board.$board.id"
fi

cd `dirname -- "$0"`

scala-cli AdventPrinter.sc -M main --scala-snippet "@main def main() = AdventPrinter.printBoard(\
  boardId=$boardArg, \
  daysToShow=$numDays, \
  usersPerDay=$numUsers, \
  hoursCutoff=$hoursCutoffArg, \
  year=$year \
)"
