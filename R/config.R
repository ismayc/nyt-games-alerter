# Settings for the NYT games alert. Everything a future edit is likely to touch
# lives here: the Wordle pattern, which crosswords are checked, and who is
# emailed.

# Underscores are wildcards, letters must match in place. "__A_E" is any
# five-letter word with A third and E fifth; "_A_E_" is A second and E fourth.
# The alerter fires when the answer fits any pattern here, and the report
# analyzes each one. Add or remove patterns in this one place.
WORDLE_PATTERNS <- c("__A_E", "_A_E_")
WORDLE_PATTERN  <- WORDLE_PATTERNS[[1]]   # the first pattern, where a single one is meant

# name = label used in the email, value = path under the v6 puzzle endpoint.
# All three serve full cell answers without a login (verified 2026-09-19).
CROSSWORDS <- c(
  "The Crossword" = "daily",
  "The Mini"      = "mini",
  "The Midi"      = "midi"
)

WORDLE_URL    <- "https://www.nytimes.com/svc/wordle/v2/%s.json"
CROSSWORD_URL <- "https://www.nytimes.com/svc/crosswords/v6/puzzle/%s/%s.json"

# Where the report is published. Used for absolute Open Graph and canonical
# URLs in the page head.
SITE_URL <- "https://nyt-games-alerter.netlify.app"

# The Netlify site the daily job publishes to. The id is public; the auth token
# is a secret and must come from the environment as NETLIFY_AUTH_TOKEN (set it
# in R/config.local.R on the publishing machine, never in the repo). Without a
# token the daily deploy is skipped and the site refreshes on the monthly job.
NETLIFY_SITE_ID <- Sys.getenv("NETLIFY_SITE_ID", "9221f80a-d120-4590-9509-fd160749e0b3")

# Puzzle dates follow the machine's local calendar, which is Phoenix.
LOCAL_TZ <- "America/Phoenix"

# Person- and machine-specific settings (the sending mailbox, the recipient,
# and the directory holding the Gmail OAuth client and token cache) are kept
# out of the repo so it can be public. Real values go in R/config.local.R,
# which is gitignored; copy R/config.local.example.R to create it. Environment
# variables override the placeholders below.
GMAIL_ACCOUNT   <- Sys.getenv("NYT_GMAIL_ACCOUNT", "sender@example.com")
ALERT_TO        <- Sys.getenv("NYT_ALERT_TO", "you@example.com")
GMAIL_TOKEN_DIR <- Sys.getenv("NYT_GMAIL_TOKEN_DIR",
                              file.path(path.expand("~"), ".nyt-games-alert"))

if (!exists("REPO_ROOT")) REPO_ROOT <- getwd()
local_config <- file.path(REPO_ROOT, "R", "config.local.R")
if (file.exists(local_config)) source(local_config, local = FALSE)

# Derived after the local overrides, so a local GMAIL_TOKEN_DIR takes effect.
GMAIL_CREDENTIALS <- file.path(GMAIL_TOKEN_DIR, "credentials.json")
GMAIL_TOKEN_BASE  <- file.path(GMAIL_TOKEN_DIR, "gmail_token")

# The history CSV and the report live in the repo folder. REPO_ROOT is set by
# the calling script; the tests and interactive use run from the repo root.
if (!exists("REPO_ROOT")) REPO_ROOT <- getwd()
HISTORY_FILE <- file.path(REPO_ROOT, "data", "history.csv")
REPORT_FILE  <- file.path(REPO_ROOT, "report", "index.html")

# State and logs stay outside the repo and outside Dropbox: they are
# per-machine facts about what this Mac has already checked and sent.
STATE_DIR  <- file.path(path.expand("~"), "Library", "Application Support", "nyt-games-alert")
STATE_FILE <- file.path(STATE_DIR, "state.json")
LOG_DIR    <- file.path(path.expand("~"), "Library", "Logs", "nyt-games-alert")
LOG_FILE   <- file.path(LOG_DIR, "check.log")

log_msg <- function(...) {
  line <- sprintf("%s %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), paste0(...))
  cat(line, "\n", sep = "")
  if (!dir.exists(LOG_DIR)) dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
  cat(line, "\n", sep = "", file = LOG_FILE, append = TRUE)
  invisible(line)
}
