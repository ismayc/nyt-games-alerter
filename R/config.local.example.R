# Copy this file to R/config.local.R and fill in your values.
# R/config.local.R is gitignored, so your email and token path stay private.
GMAIL_ACCOUNT   <- "sender@gmail.com"        # the mailbox that sends the alert
ALERT_TO        <- "you@example.com"         # where the alert is sent
# Directory holding the Gmail OAuth credentials.json and the cached token:
GMAIL_TOKEN_DIR <- file.path(path.expand("~"), ".nyt-games-alert")

# Optional: a Netlify personal access token, so the daily job publishes the
# rebuilt report to the live site each morning. The deploy uses the Netlify HTTP
# API through httr, so no Node or netlify-cli is needed. Without a token the
# daily build stays local and the site refreshes on the monthly job.
# Create one at https://app.netlify.com/user/applications#personal-access-tokens
# Sys.setenv(NETLIFY_AUTH_TOKEN = "nfp_your_token_here")
