# NYT Games Alerter

Emails Chester when a New York Times puzzle is worth a look, and keeps a
running history for analysis, published at
[nyt-games-alerter.netlify.app](https://nyt-games-alerter.netlify.app):

- **Rebus alert.** Any day the daily Crossword, the Mini, or the Midi has a
  rebus (a square whose answer is more than one letter) in its solution.
- **Wordle pattern alert.** Any day the Wordle answer fits `__A_E` (A third, E
  fifth) or `_A_E_` (A second, E fourth). The email says only that a pattern
  matched, never which one, so it gives no hint about the answer. Edit the set
  in one place: `WORDLE_PATTERNS` in `R/config.R`.
- **History and report.** Every checked day is appended to `data/history.csv`,
  and `report/index.html` is rebuilt: an interactive, scroll-driven page with
  two tabs, one for the crossword rebus and one for the Wordle pattern.

At most one combined email goes out per day. The email states the fact only:
no answers, no rebus contents, no square counts.

## The report page

`report/index.html` is a self-contained page (no external scripts, fonts, or
images; the one asset is `report/og.png`, the social card). The **Rebus** tab
covers how often the crossword hides a rebus, by weekday and year, with a
pinned decade heat grid, plus a count of *gimmick* squares (see below). The
**Wordle** tab covers how often the answer fits the pattern, plus "what makes a
Wordle nasty": repeated letters, rare letters, and the one-blank look-alike
families players complain about.

The analysis is refreshed on the first of each month by a GitHub Actions
workflow (`.github/workflows/monthly.yml`), which fetches any missing recent
days, rebuilds the report, commits it, and deploys `report/` to Netlify. It
runs in the cloud, so the published page updates even when the Mac is off. The
daily launchd job on the Mac keeps its own local copy current and sends the
email alerts.

### Rebus versus gimmick

`rebus_cells` counts squares whose answer is longer than one letter, the
classic rebus. `gimmick_cells` counts single-letter squares that still accept a
second answer, a Schrodinger clue, a symbol, or an April Fools swap, which
XWord Info also files under "rebus" but which are a different device. Ordinary
puzzles have zero of these; a purely visual gimmick (a drawn shape, circled
squares) leaves no trace in the puzzle JSON and is not counted.

## How it runs

A launchd job runs `scripts/check_today.R` three times each morning (05:07,
08:07, and 11:07 Phoenix time). The 05:07 run is the real one; the later two
exist only for a morning when the Mac was asleep or offline at 05:07. A check
that already succeeded is skipped, and a finding already emailed is never sent
again, so the extra runs are close to free.

Email is sent as the configured `GMAIL_ACCOUNT` (see `R/config.local.R`)
through `gmailr`, reusing a per-machine OAuth token cache shared with a sibling
alert job. No new credential is created and no interactive consent is needed on
a Mac where that token cache already exists. See `R/notify.R`.

```
R/
  config.R    pattern, which games, email + file locations, log helper
  detect.R    pure logic: does a Wordle match, does a grid have a rebus
  fetch.R     the NYT JSON endpoints, with ret/backoff and date checks
  state.R     per-day record of what was checked and emailed (send-once)
  notify.R    Gmail auth (copied from lottery-invoice-submission) and the email
  run.R       one pass over the day's checks
  history.R   data/history.csv: append today, backfill the last 60 days
  report.R    build report/index.html (two-tab interactive page) from the CSV
scripts/
  check_today.R       launchd entry point (alert, then history, then report)
  backfill_history.R  one-time archive download into data/backfill-*.csv
  backfill_gimmicks.R one-time re-scan of every crossword to fill gimmick_cells
  build_report.R      rebuild the report from the CSV, no network
  refresh_and_build.R fetch missing days and rebuild, no email (used by CI)
.github/workflows/
  monthly.yml         first-of-month cloud refresh, commit, and Netlify deploy
launchd/
  com.chesterismay.nyt-games-alert.plist  daily alert + rebuild on the Mac
data/
  history.csv         one row per game per date
  wordle-answers.txt  the original Wordle answer list, for the report's stats
report/
  index.html  the built page
  og.png      the social card referenced by the Open Graph tags
tests/
  testthat/          detection and run-loop tests (no network, no Gmail)
  run-tests.R
```

## The data

The NYT serves each puzzle's full solution JSON to its own game pages without
a login. A rebus is any cell whose stored `answer` is longer than one
character. Archives reach back to:

| Game | First date | Endpoint |
|------|-----------|----------|
| The Crossword | 1993-11-21 | `/svc/crosswords/v6/puzzle/daily/<date>.json` |
| The Mini | 2014-08-21 | `/svc/crosswords/v6/puzzle/mini/<date>.json` |
| The Midi | 2026-02 | `/svc/crosswords/v6/puzzle/midi/<date>.json` |
| Wordle | 2021-06-19 | `/svc/wordle/v2/<date>.json` |

`data/history.csv` records a Wordle answer only after its day has passed, so
opening the file or the report never spoils the current puzzle.

## Setup

The alert path needs R with `gmailr`, `gargle`, `httr`, and `jsonlite`, and a
Gmail OAuth token cache present for this machine. Personal settings (the
mailbox, the recipient, and the token directory) live outside the repo:

```bash
cp R/config.local.example.R R/config.local.R   # then edit the three values
```

To install the daily job, first edit the absolute paths in the plist
(`/Users/YOU/...`), then:

```bash
cp launchd/com.chesterismay.nyt-games-alert.plist ~/Library/LaunchAgents/
launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.chesterismay.nyt-games-alert.plist
```

The monthly publish runs in GitHub Actions, not on the Mac. It needs one
repository secret, a Netlify personal access token from
[Netlify user settings](https://app.netlify.com/user/applications#personal-access-tokens):

```bash
gh secret set NETLIFY_AUTH_TOKEN --repo ismayc/nyt-games-alerter
```

The site id is public and already in the workflow. Trigger a run by hand with
`gh workflow run "Monthly refresh"`; it also runs on the first of each month.
The initial deploy was done once with `netlify deploy --prod --dir report`.

Test it without waiting for the schedule:

```bash
Rscript scripts/check_today.R --dry-run                 # print, do not email
Rscript scripts/check_today.R --dry-run --date=2026-09-16   # a known rebus day
Rscript scripts/check_today.R --date=2026-09-16         # really email that day
```

## Backfilling the history

`data/history.csv` fills forward on its own, but the one-time archive download
is done with `backfill_history.R`. Each game writes its own part file so
several can run at once:

```bash
Rscript scripts/backfill_history.R --game=daily &
Rscript scripts/backfill_history.R --game=mini &
Rscript scripts/backfill_history.R --game=wordle &
wait
Rscript scripts/backfill_history.R --merge     # fold part files into history.csv
Rscript scripts/build_report.R                 # rebuild the report
```

A run only fetches dates it does not already have, so it is safe to stop and
restart. `--from=` and `--to=` narrow the range, and `--tag=` names a separate
part file so one game's range can be split across processes.

## Tests

```bash
Rscript tests/run-tests.R
```

No network and no Gmail: detection runs against small synthetic puzzles, and
the run loop uses a fake fetcher and a fake sender, so the send-once, retry,
and failure-email behavior is all checked offline.
