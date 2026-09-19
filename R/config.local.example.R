# Copy this file to R/config.local.R and fill in your values.
# R/config.local.R is gitignored, so your email and token path stay private.
GMAIL_ACCOUNT   <- "sender@gmail.com"        # the mailbox that sends the alert
ALERT_TO        <- "you@example.com"         # where the alert is sent
# Directory holding the Gmail OAuth credentials.json and the cached token:
GMAIL_TOKEN_DIR <- file.path(path.expand("~"), ".nyt-games-alert")
