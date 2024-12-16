
# Usage
The only dependency is [scala-cli](https://scala-cli.virtuslab.org/install). Get your session cookie, save it to `aoc_sesion`, and invoke `display_leaderboard.sh`:
```zsh
echo "YOUR_AOC_SESSION_COOKIE_VALUE_HERE" > aoc_session
sh scripts/display_leaderboard.sh
```
Or, with all available arguments:
```zsh
sh scripts/display_leaderboard.sh \
  --board 712467 \
  --numDays 5 \
  --numUsers 5 \
  --hoursCutoff 24 \
  --year 2024
```
