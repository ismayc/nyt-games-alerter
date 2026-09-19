# Builds report/index.html from data/history.csv: one self-contained page, no
# external scripts, fonts, or images. Rebuilt by every daily run.
#
# Needs config.R, history.R, and notify.R (for and_list) sourced first.
#
# The page is an editorial, scroll-driven read split into two tabs, one for the
# crossword rebus and one for the Wordle pattern. Everything is inline: system
# font stacks (a serif for display and big numbers, sans for body and data),
# CSS, a small favicon as an inline SVG, and vanilla JS. The one external asset
# is og.png, the social card, served from the same site. Charts are HTML and
# CSS, not SVG, so they reflow on a phone at real text size. Motion is tied to
# the data: bars grow and headline numbers count up when they scroll into view,
# and the decade heat grid pins while its readout advances. All motion is
# skipped under prefers-reduced-motion, every chart has a table view beneath it,
# and every bar and heat cell is focusable with the same tooltip on focus as on
# hover.

WEEK <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

esc <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}

pct   <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
comma <- function(x) format(x, big.mark = ",", trim = TRUE)

nice_date <- function(date) {
  d <- as.Date(date)
  sprintf("%s %d, %s", format(d, "%B"), as.integer(format(d, "%d")), format(d, "%Y"))
}

# First of next month, for the "next refresh" note.
first_of_next_month <- function(today = Sys.Date()) {
  d <- as.Date(today)
  m <- as.integer(format(d, "%m")); y <- as.integer(format(d, "%Y"))
  if (m == 12L) { m <- 1L; y <- y + 1L } else m <- m + 1L
  as.Date(sprintf("%04d-%02d-01", y, m))
}

# ---- summaries --------------------------------------------------------------

weekday_summary <- function(rows) {
  out <- data.frame(weekday = WEEK, stringsAsFactors = FALSE)
  out$puzzles <- vapply(WEEK, function(w) sum(rows$weekday == w), integer(1))
  out$hits    <- vapply(WEEK, function(w) sum(rows$hit[rows$weekday == w]), integer(1))
  out$rate    <- ifelse(out$puzzles > 0, out$hits / out$puzzles, 0)
  out
}

year_summary <- function(rows) {
  years <- sort(unique(substr(rows$date, 1, 4)))
  out <- data.frame(year = years, stringsAsFactors = FALSE)
  out$puzzles <- vapply(years, function(y) sum(substr(rows$date, 1, 4) == y), integer(1))
  out$hits    <- vapply(years, function(y) sum(rows$hit[substr(rows$date, 1, 4) == y]), integer(1))
  out$rate    <- out$hits / out$puzzles
  out
}

decade_of <- function(date) paste0(substr(date, 1, 3), "0s")

# ---- components -------------------------------------------------------------

# A headline figure in a stat band. The integer counts up on reveal; the text
# content is the final value so a no-JS reader still sees it. `accent` spends
# the one warm color on the single figure a tab is about.
stat <- function(n, label, note, accent = FALSE, comma_it = TRUE) {
  shown <- if (comma_it) comma(n) else as.character(n)
  sprintf('<div class="stat"><div class="stat-num%s" data-count="%d"%s>%s</div><div class="stat-label">%s</div><div class="stat-note">%s</div></div>',
          if (accent) " accent" else "", n, if (comma_it) " data-comma" else "",
          shown, esc(label), esc(note))
}

# A stat whose value is text (a date, say) rather than a number to count up.
stat_text <- function(value, label, note, accent = FALSE) {
  sprintf('<div class="stat"><div class="stat-num tx%s">%s</div><div class="stat-label">%s</div><div class="stat-note">%s</div></div>',
          if (accent) " accent" else "", esc(value), esc(label), esc(note))
}

# The Wordle target pattern as a row of tiles: fixed letters filled in the
# "correct" green, blanks left as open squares.
wordle_tiles <- function(pattern, big = FALSE) {
  ch <- strsplit(pattern, "")[[1]]
  tiles <- vapply(ch, function(c) {
    if (c == "_") '<div class="wtile"></div>'
    else sprintf('<div class="wtile fixed">%s</div>', esc(c))
  }, character(1))
  sprintf('<div class="wtiles%s" role="img" aria-label="Wordle target pattern %s">%s</div>',
          if (big) " big" else "", esc(pattern), paste(tiles, collapse = ""))
}

# One column per label. `caps` are labels drawn on top of a column; pass "" to
# leave one off. `peak` marks the accent-colored columns; the rest are neutral.
# Bar heights ride a --h custom property so they grow from zero on reveal.
column_chart <- function(labels, values, caps, tips, tick_format, aria,
                         dense = FALSE, peak = rep(FALSE, length(values))) {
  ticks <- pretty(c(0, max(values) * 1.18), n = 4)
  top   <- max(ticks)
  if (top == 0) top <- 1
  grid <- paste(sprintf(
    '<div class="gridline" style="bottom:%.3f%%"><span>%s</span></div>',
    100 * ticks / top, vapply(ticks, tick_format, character(1))), collapse = "")
  cols <- paste(sprintf(
    '<div class="col%s" tabindex="0" data-tip="%s"><div class="cap">%s</div><div class="bar" style="--h:%.3f%%"></div></div>',
    ifelse(peak, " peak", ""), esc(tips), esc(caps), 100 * values / top), collapse = "")
  axis <- paste(sprintf('<div class="xlabel">%s</div>', esc(labels)), collapse = "")
  sprintf('<div class="chart%s" role="group" aria-label="%s"><div class="plot">%s<div class="cols">%s</div></div><div class="xaxis">%s</div></div>',
          if (dense) " dense" else "", esc(aria), grid, cols, axis)
}

html_table <- function(df, headers, numeric_cols = integer()) {
  head <- paste(sprintf("<th%s>%s</th>",
                        ifelse(seq_along(headers) %in% numeric_cols, ' class="num"', ""),
                        esc(headers)), collapse = "")
  body <- paste(apply(df, 1, function(r) {
    paste0("<tr>", paste(sprintf("<td%s>%s</td>",
                                 ifelse(seq_along(r) %in% numeric_cols, ' class="num"', ""),
                                 esc(r)), collapse = ""), "</tr>")
  }), collapse = "")
  sprintf('<div class="table-wrap"><table><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>', head, body)
}

table_view <- function(table_html) {
  sprintf("<details><summary>Table view</summary>%s</details>", table_html)
}

# Rebus rate for each weekday within each decade. Cell color is the accent hue
# mixed toward the surface in proportion to the rate. Rows carry data-decade so
# the scrolly readout can highlight one decade at a time.
decade_grid <- function(rows) {
  decades <- sort(unique(decade_of(rows$date)))
  rate <- function(w, d) {
    sel <- rows$weekday == w & decade_of(rows$date) == d
    c(hits = sum(rows$hit[sel]), n = sum(sel))
  }
  cells <- lapply(decades, function(d) lapply(WEEK, function(w) rate(w, d)))
  top   <- max(vapply(unlist(cells, recursive = FALSE),
                      function(x) if (x[["n"]] > 0) x[["hits"]] / x[["n"]] else 0, numeric(1)))
  if (top == 0) top <- 1
  head <- paste0("<th></th>", paste(sprintf('<th class="num">%s</th>', substr(WEEK, 1, 3)), collapse = ""))
  body <- paste(vapply(seq_along(decades), function(i) {
    tds <- vapply(seq_along(WEEK), function(j) {
      x <- cells[[i]][[j]]
      r <- if (x[["n"]] > 0) x[["hits"]] / x[["n"]] else 0
      mix <- round(100 * r / top * 0.9)
      sprintf('<td class="heat%s" tabindex="0" style="--mix:%d%%" data-tip="%s">%s</td>',
              if (mix >= 55) " hot" else "", mix,
              esc(sprintf("%s|%s, %s: %d of %s puzzles", pct(r), WEEK[j], decades[i],
                          x[["hits"]], comma(x[["n"]]))),
              if (x[["n"]] > 0) pct(r, 0) else "")
    }, character(1))
    sprintf('<tr data-decade="%s"><th>%s</th>%s</tr>', decades[i], decades[i], paste(tds, collapse = ""))
  }, character(1)), collapse = "")
  sprintf('<div class="table-wrap"><table class="grid" data-scrolly><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>', head, body)
}

# A small inline-SVG data URI for the favicon: a crossword 2x2 with two accent
# squares on the diagonal. Encoded minimally (single quotes inside the SVG).
FAVICON <- {
  svg <- paste0("<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 32 32'>",
                "<rect width='32' height='32' rx='7' fill='%23161a22'/>",
                "<rect x='4.5' y='4.5' width='10' height='10' rx='2.5' fill='%23e8560d'/>",
                "<rect x='17.5' y='4.5' width='10' height='10' rx='2.5' fill='%233a4049'/>",
                "<rect x='4.5' y='17.5' width='10' height='10' rx='2.5' fill='%233a4049'/>",
                "<rect x='17.5' y='17.5' width='10' height='10' rx='2.5' fill='%23e8560d'/></svg>")
  paste0("data:image/svg+xml,", gsub(" ", "%20", svg, fixed = TRUE))
}

MARK <- paste0('<svg class="mark" viewBox="0 0 32 32" width="22" height="22" aria-hidden="true">',
               '<rect width="32" height="32" rx="7" fill="currentColor" opacity=".12"/>',
               '<rect x="4.5" y="4.5" width="10" height="10" rx="2.5" fill="var(--accent)"/>',
               '<rect x="17.5" y="4.5" width="10" height="10" rx="2.5" fill="currentColor" opacity=".55"/>',
               '<rect x="4.5" y="17.5" width="10" height="10" rx="2.5" fill="currentColor" opacity=".55"/>',
               '<rect x="17.5" y="17.5" width="10" height="10" rx="2.5" fill="var(--accent)"/></svg>')

# GitHub-style heatmap for one year: 7 rows (Sun..Sat) by week. Cell color
# diverges by vs_weekday: the cool hue for easier than a normal weekday, the
# accent for harder. dmap is an environment keyed by date -> list(vs, label).
diff_year_heatmap <- function(year, dmap) {
  jan1  <- as.Date(sprintf("%d-01-01", year))
  last  <- min(as.Date(sprintf("%d-12-31", year)), Sys.Date())
  days  <- seq(jan1, last, by = "day")
  pad   <- as.integer(format(jan1, "%w"))            # 0=Sun: leading blanks
  slots <- c(rep(NA_character_, pad), format(days))
  cells <- vapply(slots, function(ds) {
    if (is.na(ds)) return('<div class="hcell pad"></div>')
    e <- if (exists(ds, envir = dmap, inherits = FALSE)) get(ds, envir = dmap) else NULL
    if (is.null(e)) {
      return(sprintf('<div class="hcell" tabindex="0" data-tip="%s"></div>',
                     esc(sprintf("No data yet|%s", nice_date(ds)))))
    }
    mix <- min(88L, as.integer(round(abs(e$vs) * 2.4)))
    hue <- if (e$vs > 0) "--accent" else "--cool"
    tip <- esc(sprintf("%s|%s, %d%% %s than a typical %s", e$label, nice_date(ds),
                       abs(e$vs), if (e$vs > 0) "slower" else "faster", format(as.Date(ds), "%A")))
    sprintf('<div class="hcell" tabindex="0" style="--c:color-mix(in srgb,var(%s) %d%%,var(--surface-2))" data-tip="%s"></div>',
            hue, mix, tip)
  }, character(1))
  sprintf('<div class="hyear"><div class="hylabel">%d</div><div class="hgrid">%s</div></div>',
          year, paste(cells, collapse = ""))
}

# ---- page -------------------------------------------------------------------

REPORT_CSS <- '
:root{
color-scheme:light;
--serif:"Iowan Old Style","Palatino Linotype",Palatino,"Book Antiqua",Georgia,"Times New Roman",serif;
--sans:system-ui,-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;
--page:#f3f4f6;--surface:#ffffff;--surface-2:#fafbfc;
--ink:#161a22;--ink-2:#565d6b;--muted:#8b93a2;
--line:#e4e7ec;--axis:#c8cdd6;--bar-neutral:#c6ccd6;
--accent:#e8560d;--on-accent:#ffffff;--green:#4f9d5a;--hot-ink:#ffffff;--cool:#2f77c0;
--shadow:0 1px 2px rgba(22,26,34,.05),0 18px 40px -24px rgba(22,26,34,.4);
}
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]){
color-scheme:dark;
--page:#0e1014;--surface:#161a20;--surface-2:#1b2028;
--ink:#f2f3f6;--ink-2:#aab0bc;--muted:#6f7787;
--line:#272c35;--axis:#3a4049;--bar-neutral:#39404b;
--accent:#ff7a33;--on-accent:#20120a;--green:#5cae67;--hot-ink:#ffffff;--cool:#4a97dd;
--shadow:0 1px 2px rgba(0,0,0,.4),0 22px 48px -26px rgba(0,0,0,.75);
}}
:root[data-theme="dark"]{
color-scheme:dark;
--page:#0e1014;--surface:#161a20;--surface-2:#1b2028;
--ink:#f2f3f6;--ink-2:#aab0bc;--muted:#6f7787;
--line:#272c35;--axis:#3a4049;--bar-neutral:#39404b;
--accent:#ff7a33;--on-accent:#20120a;--green:#5cae67;--hot-ink:#ffffff;--cool:#4a97dd;
--shadow:0 1px 2px rgba(0,0,0,.4),0 22px 48px -26px rgba(0,0,0,.75);
}
*{box-sizing:border-box}
html{scroll-behavior:smooth}
body{margin:0;background:var(--page);color:var(--ink);font-family:var(--sans);font-size:17px;line-height:1.6;-webkit-font-smoothing:antialiased;text-rendering:optimizeLegibility}
main{max-width:940px;margin:0 auto;padding:0 22px 40px}
h1,h2,h3{font-family:var(--serif);font-weight:600;letter-spacing:-.012em;color:var(--ink)}
p{margin:0 0 12px}
b{font-weight:600}
a{color:var(--accent);text-underline-offset:2px}

/* top bar + tabs */
.topbar{position:sticky;top:0;z-index:20;display:flex;align-items:center;justify-content:space-between;gap:16px;padding:12px 0;margin-bottom:8px;background:color-mix(in srgb,var(--page) 86%,transparent);backdrop-filter:blur(10px);border-bottom:1px solid var(--line)}
.brand{display:flex;align-items:center;gap:9px;font-family:var(--serif);font-weight:600;font-size:16px;color:var(--ink)}
.brand .mark{flex:0 0 auto;color:var(--ink)}
.tabs{display:flex;gap:4px;background:var(--surface-2);border:1px solid var(--line);border-radius:999px;padding:4px}
.tab{appearance:none;border:0;background:transparent;color:var(--ink-2);font:inherit;font-size:14px;font-weight:600;padding:7px 15px;border-radius:999px;cursor:pointer}
.tab[aria-selected="true"]{background:var(--accent);color:var(--on-accent)}
.tab:focus-visible{outline:2px solid var(--accent);outline-offset:2px}
.panel[hidden]{display:none}
.panel+.panel{border-top:1px solid var(--line)}

/* hero */
.hero{padding:64px 0 8px}
.hero-title{font-size:clamp(40px,8vw,76px);line-height:1.03;margin:0 0 22px;max-width:15ch}
.lede{font-size:clamp(19px,2.4vw,24px);line-height:1.5;color:var(--ink-2);max-width:52ch;margin:0}
.lede b{color:var(--ink)}
.js .hero-title{animation:rise .8s .05s both}
.js .lede{animation:rise .8s .2s both}
@keyframes rise{from{opacity:0;transform:translateY(18px)}to{opacity:1;transform:none}}
.whero{padding:52px 0 8px}
.whero .wtiles{margin-bottom:28px}

/* stat band */
.stats{display:grid;grid-template-columns:repeat(4,1fr);gap:1px;background:var(--line);border:1px solid var(--line);border-radius:16px;overflow:hidden;margin:44px 0 0;box-shadow:var(--shadow)}
.stat{background:var(--surface);padding:24px 22px}
.stat-num{font-family:var(--serif);font-weight:600;font-size:clamp(32px,5vw,48px);line-height:1;font-variant-numeric:tabular-nums}
.stat-num.tx{font-size:clamp(22px,3.4vw,30px)}
.stat-num.accent{color:var(--accent)}
.stat-label{margin-top:12px;font-size:15px;font-weight:600}
.stat-note{margin-top:3px;font-size:13.5px;color:var(--muted)}
@media(max-width:640px){.stats{grid-template-columns:repeat(2,1fr)}}

/* sections */
section{padding:60px 0;border-top:1px solid var(--line)}
.hero+section,.whero+section{border-top:0}
.section-head{max-width:60ch}
h2{font-size:clamp(26px,3.8vw,38px);line-height:1.12;margin:0 0 14px}
h3{font-size:19px;margin:0 0 4px}
.sub{color:var(--ink-2);max-width:60ch}
.facts{list-style:none;margin:20px 0 0;padding:0;max-width:62ch}
.facts li{padding:14px 0;border-top:1px solid var(--line);color:var(--ink-2)}
.facts li:first-child{border-top:0}
.facts b{color:var(--ink)}
.tag{display:inline-block;font-size:12.5px;font-weight:600;color:var(--accent);background:color-mix(in srgb,var(--accent) 12%,var(--surface));border:1px solid color-mix(in srgb,var(--accent) 30%,var(--surface));padding:2px 9px;border-radius:999px;margin-bottom:10px}

/* charts */
.chart{margin-top:28px}
.plot{position:relative;height:300px;margin-left:48px;border-bottom:1px solid var(--axis)}
.gridline{position:absolute;left:0;right:0;height:0;border-top:1px solid var(--line)}
.gridline:first-child{border-top-color:transparent}
.gridline span{position:absolute;right:100%;margin-right:8px;transform:translateY(-50%);font-size:12px;color:var(--muted);font-variant-numeric:tabular-nums;white-space:nowrap}
.cols{position:absolute;inset:0;display:flex;align-items:stretch;gap:3px}
.col{flex:1 1 0;min-width:0;display:flex;flex-direction:column;justify-content:flex-end;align-items:center;outline:none;cursor:default}
.bar{width:100%;max-width:38px;min-height:2px;height:var(--h);background:var(--bar-neutral);border-radius:5px 5px 0 0;transition:height .95s cubic-bezier(.2,.7,.2,1),filter .15s}
.col.peak .bar{background:var(--accent)}
.js .bar{height:0}
.js .chart.in .bar{height:var(--h)}
.col:hover .bar,.col:focus-visible .bar{filter:brightness(1.12)}
.col:focus-visible{box-shadow:inset 0 0 0 2px var(--axis);border-radius:5px}
.cap{font-size:12.5px;color:var(--ink-2);margin-bottom:5px;white-space:nowrap;font-variant-numeric:tabular-nums}
.col.peak .cap{color:var(--accent);font-weight:600}
.xaxis{display:flex;gap:3px;margin-left:48px;padding-top:7px}
.xlabel{flex:1 1 0;min-width:0;text-align:center;font-size:12.5px;color:var(--muted);white-space:nowrap}

/* pinned decade scrolly */
.scrolly{border-top:1px solid var(--line);padding:60px 0}
.scrolly>.section-head{margin-bottom:8px}
.scrolly-grid{display:grid;grid-template-columns:minmax(0,1fr) 300px;gap:44px;align-items:start}
.scrolly-viz{position:sticky;top:80px}
.scrolly-steps{display:flex;flex-direction:column;gap:52vh;padding:26vh 0}
.step{opacity:.32;transition:opacity .45s}
.step.on{opacity:1}
.step .k{font-family:var(--serif);font-size:34px;color:var(--accent);line-height:1}
.step p{margin:8px 0 0;color:var(--ink-2)}
.grid[data-active] tbody tr:not(.on){opacity:.34;transition:opacity .35s}
.grid tbody tr.on th{color:var(--accent)}
@media(max-width:760px){.scrolly-grid{grid-template-columns:1fr;gap:24px}.scrolly-viz{position:static}.scrolly-steps{gap:28px;padding:8px 0 0}.step{opacity:1}.grid[data-active] tbody tr:not(.on){opacity:1}}

/* wordle tiles */
.wtiles{display:flex;gap:8px;margin:0 0 22px}
.wtile{width:54px;height:54px;border-radius:9px;display:flex;align-items:center;justify-content:center;font-family:var(--serif);font-size:27px;font-weight:600;border:2px solid var(--line);color:var(--muted);background:var(--surface)}
.wtile.fixed{background:var(--green);border-color:var(--green);color:#fff}
.wtiles.big .wtile{width:66px;height:66px;font-size:33px;border-radius:11px}
@media(max-width:420px){.wtile{width:46px;height:46px;font-size:23px}.wtiles.big .wtile{width:54px;height:54px;font-size:27px}}

/* tables, details, word list */
details{margin-top:16px}
summary{cursor:pointer;color:var(--ink-2);font-size:15px}
summary:hover{color:var(--accent)}
.table-wrap{overflow-x:auto;margin-top:10px}
table{border-collapse:collapse;width:100%;font-size:14.5px}
th,td{text-align:left;padding:7px 12px;border-bottom:1px solid var(--line)}
th{color:var(--ink-2);font-weight:600}
.num{text-align:right;font-variant-numeric:tabular-nums}
table.grid{min-width:340px}
table.grid td,table.grid th{border-bottom:0}
table.grid td.heat{text-align:right;font-variant-numeric:tabular-nums;background:color-mix(in srgb,var(--accent) var(--mix),var(--surface));border:2px solid var(--surface);border-radius:7px;outline:none}
table.grid td.heat.hot{color:var(--hot-ink)}
table.grid td.heat:hover,table.grid td.heat:focus-visible{border-color:var(--axis)}
.words{columns:150px;column-gap:24px;margin:10px 0 0;padding:0;list-style:none;font-size:14px}
.words li{break-inside:avoid;padding:2px 0}
.words b{font-weight:600;letter-spacing:.04em}
.words span{color:var(--muted);margin-left:6px;font-variant-numeric:tabular-nums}
.note{color:var(--muted);font-size:13.5px}

/* footer */
footer{border-top:1px solid var(--line);padding:44px 0 12px;color:var(--ink-2)}
.foot-cols{display:flex;flex-wrap:wrap;gap:30px 48px}
.foot-cols h4{font-family:var(--sans);font-size:13px;font-weight:700;color:var(--ink);margin:0 0 10px}
.foot-cols ul{list-style:none;margin:0;padding:0;font-size:14.5px}
.foot-cols li{margin:6px 0}
footer a{color:var(--ink);text-decoration:none;border-bottom:1px solid var(--line)}
footer a:hover{border-color:var(--accent);color:var(--accent)}
.foot-note{margin-top:28px;padding-top:16px;border-top:1px solid var(--line);color:var(--muted);font-size:13px;display:flex;flex-wrap:wrap;gap:6px 16px;justify-content:space-between}

/* day lookup */
.lookup{margin-top:24px;max-width:60ch}
.lookup input[type=date]{font:inherit;font-size:16px;padding:12px 14px;border:1px solid var(--line);border-radius:12px;background:var(--surface);color:var(--ink);width:100%;max-width:280px}
.lookup input[type=date]:focus-visible{outline:2px solid var(--accent);outline-offset:2px;border-color:var(--accent)}
.lk-result{margin-top:16px;padding:18px 20px;border:1px solid var(--line);border-radius:12px;background:var(--surface-2);font-size:17px;line-height:1.5}
.lk-result b{color:var(--ink)}
.lk-result.yes{border-color:color-mix(in srgb,var(--accent) 45%,var(--line))}
.lk-result.yes b:first-child{color:var(--accent)}
.lk-result:empty{display:none}

/* difficulty calendar */
.cal-wrap{overflow-x:auto;margin-top:20px;padding-bottom:6px}
.hyear{display:flex;gap:16px;align-items:center;margin:9px 0}
.hylabel{width:46px;flex:0 0 auto;font-variant-numeric:tabular-nums;color:var(--ink-2);font-size:14px;font-weight:600}
.hgrid{display:grid;grid-template-rows:repeat(7,13px);grid-auto-flow:column;grid-auto-columns:13px;gap:3px}
.hcell{width:13px;height:13px;border-radius:3px;background:var(--c,var(--surface-2));border:1px solid var(--line);outline:none}
.hcell.pad{background:transparent;border:0}
.hcell[data-tip]:hover,.hcell[data-tip]:focus-visible{box-shadow:0 0 0 2px var(--axis)}
.cal-legend{display:flex;align-items:center;gap:8px;margin-top:16px;font-size:13px;color:var(--ink-2);flex-wrap:wrap}
.cal-legend .sw{width:15px;height:15px;border-radius:3px;border:1px solid var(--line)}
.cal-src{margin-top:14px;font-size:13px;color:var(--muted)}
.cal-src a{color:var(--muted);border-bottom:1px solid var(--line)}
.cal-src a:hover{color:var(--accent);border-color:var(--accent)}

/* tooltip */
#tip{position:fixed;z-index:30;pointer-events:none;background:var(--surface);color:var(--ink);border:1px solid var(--line);border-radius:9px;padding:7px 11px;font-size:13px;box-shadow:var(--shadow);max-width:260px;display:none}
#tip b{display:block;font-size:15px}
#tip span{color:var(--ink-2)}

/* reveal + reduced motion */
.js [data-reveal]{opacity:0;transform:translateY(16px);transition:opacity .7s,transform .7s}
.js [data-reveal].in{opacity:1;transform:none}
@media (prefers-reduced-motion:reduce){
*{animation:none!important;transition:none!important;scroll-behavior:auto!important}
.js [data-reveal]{opacity:1;transform:none}
.js .bar{height:var(--h)}
}
'

# data-tip is "value|label". Text is inserted with textContent only.
REPORT_JS <- '
(function(){
var D=document,root=D.documentElement;root.classList.add("js");
var reduce=matchMedia("(prefers-reduced-motion:reduce)").matches;

var tip=D.getElementById("tip"),tv=tip.querySelector("b"),tl=tip.querySelector("span");
function show(el,x,y){var p=el.getAttribute("data-tip").split("|");tv.textContent=p[0];tl.textContent=p.slice(1).join("|");tip.style.display="block";var w=tip.offsetWidth,h=tip.offsetHeight;tip.style.left=Math.max(8,Math.min(x+14,innerWidth-w-8))+"px";tip.style.top=Math.max(8,y-h-12)+"px";}
function hide(){tip.style.display="none";}
D.querySelectorAll("[data-tip]").forEach(function(el){
el.addEventListener("pointermove",function(e){show(el,e.clientX,e.clientY);});
el.addEventListener("pointerleave",hide);
el.addEventListener("focus",function(){var r=el.getBoundingClientRect();show(el,r.left+r.width/2,r.top);});
el.addEventListener("blur",hide);});

function ease(t){return 1-Math.pow(1-t,3);}
function count(el){
if(el.dataset.done)return;el.dataset.done="1";
var target=+el.getAttribute("data-count"),useComma=el.hasAttribute("data-comma");
function fmt(n){return useComma?n.toLocaleString("en-US"):String(n);}
if(reduce){el.textContent=fmt(target);return;}
var t0=null,dur=950;
function step(ts){if(!t0)t0=ts;var p=Math.min(1,(ts-t0)/dur);el.textContent=fmt(Math.round(ease(p)*target));if(p<1)requestAnimationFrame(step);}
requestAnimationFrame(step);
}

var io=new IntersectionObserver(function(es){es.forEach(function(e){
if(!e.isIntersecting)return;var el=e.target;el.classList.add("in");
el.querySelectorAll("[data-count]").forEach(count);
io.unobserve(el);
});},{threshold:.25});
D.querySelectorAll("[data-reveal],.chart").forEach(function(el){io.observe(el);});

var grid=D.querySelector(".grid[data-scrolly]"),steps=D.querySelectorAll(".scrolly-steps .step");
if(grid&&steps.length){
function setDecade(dec){grid.setAttribute("data-active",dec);grid.querySelectorAll("tbody tr").forEach(function(tr){tr.classList.toggle("on",tr.getAttribute("data-decade")===dec);});}
var so=new IntersectionObserver(function(es){es.forEach(function(e){
if(e.isIntersecting){steps.forEach(function(s){s.classList.remove("on");});e.target.classList.add("on");setDecade(e.target.getAttribute("data-decade"));}
});},{rootMargin:"-45% 0px -45% 0px"});
steps.forEach(function(s){so.observe(s);});
}

var RB=window.RB_DATA,lkI=D.getElementById("lk-date"),lkO=D.getElementById("lk-result");
if(RB&&lkI&&lkO){
var fmt=function(iso){var dt=new Date(iso+"T00:00:00");return dt.toLocaleDateString("en-US",{weekday:"long",year:"numeric",month:"long",day:"numeric"});};
var look=function(){
var d=lkI.value;if(!/^[0-9]{4}-[0-9]{2}-[0-9]{2}$/.test(d)){lkO.className="lk-result";lkO.innerHTML="";return;}
var yes=false,html;
if(d<RB.min||d>RB.max){html="<b>Outside the data.</b> Days run "+fmt(RB.min)+" to "+fmt(RB.max)+".";}
else{var e=RB.days[d];
if(e&&e[0]>0){yes=true;html="<b>Yes, a rebus.</b> "+fmt(d)+" had <b>"+e[0]+"</b> rebus square"+(e[0]==1?"":"s")+(e[1]>0?", plus "+e[1]+" alternate-answer square"+(e[1]==1?"":"s"):"")+".";}
else if(e&&e[1]>0){html="<b>No rebus,</b> but a gimmick: "+fmt(d)+" had <b>"+e[1]+"</b> alternate-answer square"+(e[1]==1?"":"s")+".";}
else{html="<b>No rebus.</b> "+fmt(d)+" was a clean grid.";}}
lkO.className="lk-result"+(yes?" yes":"");lkO.innerHTML=html;};
lkI.addEventListener("input",look);lkI.addEventListener("change",look);look();
}

var tabs=D.querySelectorAll(".tab"),panels={};
D.querySelectorAll(".panel").forEach(function(p){panels[p.id]=p;});
function activate(name,push){
if(!panels["panel-"+name])name="rebus";
tabs.forEach(function(t){t.setAttribute("aria-selected",String(t.getAttribute("data-tab")===name));});
Object.keys(panels).forEach(function(k){panels[k].hidden=(k!=="panel-"+name);});
if(push!==false&&"replaceState" in history)history.replaceState(null,"","#"+name);
window.scrollTo(0,0);
}
tabs.forEach(function(t){t.addEventListener("click",function(){activate(t.getAttribute("data-tab"));});});
activate((location.hash||"#rebus").slice(1),false);
})();
'

build_report <- function(history_path = HISTORY_FILE, out = REPORT_FILE) {
  history <- read_history(history_path)
  history <- history[order(history$date), ]   # gap and "most recent" math need date order
  daily   <- history[history$game == "daily", ]
  wordle  <- history[history$game == "wordle", ]
  if (!nrow(daily) || !nrow(wordle)) {
    log_msg("[report] not enough history yet; skipped")
    return(invisible(NULL))
  }

  # -- crossword ---------------------------------------------------------------
  by_day   <- weekday_summary(daily)
  by_year  <- year_summary(daily)
  rebuses  <- daily[daily$hit, ]
  most     <- by_day$weekday[by_day$rate == max(by_day$rate)]
  least    <- by_day$weekday[by_day$rate == min(by_day$rate)]
  expected <- as.integer(as.Date(max(daily$date)) - as.Date(min(daily$date))) + 1L
  gaps     <- diff(as.Date(rebuses$date))
  biggest  <- rebuses[which.max(rebuses$rebus_cells), ]
  span_years <- floor(as.integer(as.Date(max(daily$date)) - as.Date(min(daily$date))) / 365.25)
  one_in     <- round(1 / mean(daily$hit))

  weekday_title <- sprintf("%s %s the most rebuses, %s the fewest",
                           and_list(most), if (length(most) > 1) "have" else "has", and_list(least))
  weekday_chart <- column_chart(
    labels = substr(by_day$weekday, 1, 3), values = by_day$rate,
    caps   = pct(by_day$rate),
    tips   = sprintf("%s|%s: %d rebus puzzles out of %s", pct(by_day$rate), by_day$weekday,
                     by_day$hits, comma(by_day$puzzles)),
    tick_format = function(t) pct(t, 0),
    aria = "Share of daily crosswords with a rebus, by day of the week",
    peak = by_day$rate == max(by_day$rate))
  weekday_table <- html_table(
    data.frame(by_day$weekday, comma(by_day$puzzles), by_day$hits, pct(by_day$rate)),
    c("Day", "Puzzles", "With a rebus", "Share"), numeric_cols = 2:4)

  ypeak <- which.max(by_year$hits)
  year_chart <- column_chart(
    labels = ifelse(as.integer(by_year$year) %% 5 == 0, by_year$year, ""),
    values = by_year$hits,
    caps   = ifelse(seq_len(nrow(by_year)) == ypeak, by_year$hits, ""),
    tips   = sprintf("%d|%s: rebus puzzles out of %d (%s)", by_year$hits, by_year$year,
                     by_year$puzzles, pct(by_year$rate)),
    tick_format = function(t) format(t),
    aria = "Number of daily crosswords with a rebus, by year", dense = TRUE,
    peak = seq_len(nrow(by_year)) == ypeak)
  year_table <- html_table(
    data.frame(by_year$year, by_year$puzzles, by_year$hits, pct(by_year$rate)),
    c("Year", "Puzzles", "With a rebus", "Share"), numeric_cols = 2:4)

  # -- decade scrolly ----------------------------------------------------------
  decades <- sort(unique(decade_of(daily$date)))
  dec_stat <- function(d) {
    sel <- decade_of(daily$date) == d
    r   <- vapply(WEEK, function(w) { s <- sel & daily$weekday == w; if (any(s)) mean(daily$hit[s]) else 0 }, numeric(1))
    list(top = WEEK[which.max(r)], top_rate = max(r), thu = r[["Thursday"]], sun = r[["Sunday"]], n = sum(sel))
  }
  ds <- lapply(decades, dec_stat); names(ds) <- decades
  steps <- paste(vapply(seq_along(decades), function(i) {
    d <- decades[i]; s <- ds[[i]]
    sprintf('<div class="step" data-decade="%s"><div class="k">%s</div><p>%s led at <b>%s</b>. Thursday sat at %s, Sunday at %s, across %s puzzles.</p></div>',
            d, d, s$top, pct(s$top_rate), pct(s$thu), pct(s$sun), comma(s$n))
  }, character(1)), collapse = "")
  thu_rising <- all(diff(vapply(ds, function(s) s$thu, numeric(1))) >= 0)
  scrolly_dek <- sprintf(
    "%s had the highest rebus share in the %s. Thursday overtook it and now leads the %s at %s%s.",
    ds[[1]]$top, decades[1], decades[length(decades)], pct(ds[[length(ds)]]$thu),
    if (thu_rising) ", climbing every decade in between" else "")

  # -- gimmick squares (from the backfilled column) ----------------------------
  have_g   <- any(!is.na(daily$gimmick_cells))
  g_scanned <- sum(!is.na(daily$gimmick_cells))
  gim      <- daily[!is.na(daily$gimmick_cells) & daily$gimmick_cells > 0, ]
  gim      <- gim[order(gim$date), ]
  recent_g <- utils::tail(gim, 10)
  gimmick_section <- if (have_g && nrow(gim)) {
    dates <- paste(sprintf("<li><b>%s</b><span>%d square%s</span></li>",
                           esc(rev(recent_g$date)), rev(recent_g$gimmick_cells),
                           ifelse(rev(recent_g$gimmick_cells) == 1, "", "s")), collapse = "")
    paste0(
      '<section><div class="section-head"><span class="tag">Not counted as a rebus</span>',
      '<h2>Puzzles that cheat a different way</h2>',
      sprintf('<p class="sub">Some puzzles hide the trick inside a single square that quietly accepts a second answer, a Schrodinger clue, a symbol, or an April Fools swap, rather than cramming several letters in. Those are a different device from a rebus, so they are tracked on their own: <b>%s</b> of the %s daily puzzles scanned have at least one.</p></div>',
              comma(nrow(gim)), comma(g_scanned)),
      '<h3 style="margin-top:22px">Most recent, newest first</h3>',
      '<ul class="words">', dates, '</ul>',
      '<p class="note" style="margin-top:16px">A purely visual gimmick, a drawn shape or circled squares, leaves no mark in the puzzle data and is not caught here.</p>',
      '</section>')
  } else ""

  # -- wordle: every tracked pattern, all derived from the stored solution -----
  patterns <- WORDLE_PATTERNS
  w_known  <- wordle[!is.na(wordle$solution), ]
  pat_hit  <- function(p) grepl(pattern_to_regex(p), w_known$solution)
  hits_by_pat <- lapply(patterns, pat_hit); names(hits_by_pat) <- patterns
  either   <- Reduce(`|`, hits_by_pat)
  w_either <- w_known[either, ]
  n_either <- sum(either)
  either_one_in <- round(nrow(w_known) / max(n_either, 1))
  pat_info <- lapply(patterns, function(p) {
    rows <- w_known[hits_by_pat[[p]], ]; rows <- rows[order(rows$date), ]
    list(pattern = p, rows = rows, n = nrow(rows), rate = nrow(rows) / nrow(w_known),
         recent = if (nrow(rows)) max(rows$date) else NA,
         gaps = if (nrow(rows) > 1) diff(as.Date(rows$date)) else numeric(0),
         one_in = round(nrow(w_known) / max(nrow(rows), 1)))
  })
  names(pat_info) <- patterns

  # per-year counts: "fit either" for the chart, each pattern for the table
  yrs    <- sort(unique(substr(w_known$date, 1, 4)))
  ycount <- function(sel) vapply(yrs, function(y) sum(sel & substr(w_known$date, 1, 4) == y), integer(1))
  y_tot    <- vapply(yrs, function(y) sum(substr(w_known$date, 1, 4) == y), integer(1))
  y_either <- ycount(either)
  wpeak <- which.max(y_either)
  w_year_chart <- column_chart(
    labels = yrs, values = y_either, caps = y_either,
    tips   = sprintf("%d|%s: fit a pattern out of %d", y_either, yrs, y_tot),
    tick_format = function(t) format(t),
    aria = "Number of Wordle answers fitting a tracked pattern, by year",
    peak = seq_along(yrs) == wpeak)
  y_cols <- lapply(patterns, function(p) ycount(hits_by_pat[[p]]))
  w_year_table <- html_table(
    do.call(data.frame, c(list(yrs, y_tot), y_cols, list(y_either))),
    c("Year", "Wordles", paste("Fit", patterns), "Fit either"),
    numeric_cols = 2:(3 + length(patterns)))

  # Prefer the copy vendored into this repo so a CI build has it too; fall back
  # to the sibling wordle-helper repo for local development.
  answers_file <- file.path(REPO_ROOT, "data", "wordle-answers.txt")
  if (!file.exists(answers_file)) answers_file <- file.path(REPO_ROOT, "..", "wordle-helper", "answers.txt")
  answer_list <- if (file.exists(answers_file)) {
    a <- toupper(readLines(answers_file, warn = FALSE)); a[nchar(a) == 5]
  } else character(0)
  baseline <- if (length(answer_list)) {
    parts <- vapply(patterns, function(p) sprintf("%s fit %s",
                    pct(mean(grepl(pattern_to_regex(p), answer_list))), p), character(1))
    sprintf(" In the original %s-word answer list, %s, so these are about as common as the list predicts.",
            comma(length(answer_list)), and_list(parts))
  } else ""

  # -- wordle gripes: traits players complain about, over every answer on record
  sol      <- wordle$solution[!is.na(wordle$solution)]
  maxrep   <- vapply(sol, function(w) max(table(strsplit(w, "")[[1]])), integer(1))
  n_double <- sum(maxrep >= 2); n_triple <- sum(maxrep >= 3)
  triple_ex <- utils::head(unique(sol[maxrep >= 3]), 6)
  rare      <- grepl("[JQXZ]", sol)
  rare_ex   <- utils::head(unique(sol[rare]), 6)
  # "one blank, many options": for each answer, the most look-alikes it has in
  # the original answer list when any single letter is left open.
  fam <- if (length(answer_list)) {
    vapply(sol, function(w) {
      ch <- strsplit(w, "")[[1]]
      max(vapply(seq_len(5), function(i) {
        ch2 <- ch; ch2[i] <- "."
        sum(grepl(paste0("^", paste(ch2, collapse = ""), "$"), answer_list))
      }, integer(1)))
    }, integer(1))
  } else rep(NA_integer_, length(sol))
  worst_i   <- if (all(is.na(fam))) NA else which.max(fam)
  gripes_section <- {
    reps <- sprintf("<li>Repeated a letter: <b>%d</b> answers (%s). <b>%d</b> used one letter three times%s.</li>",
                    n_double, pct(n_double / length(sol)), n_triple,
                    if (length(triple_ex)) paste0(", like ", paste(triple_ex, collapse = ", ")) else "")
    rr <- sprintf("<li>Leaned on a rare letter (J, Q, X, or Z): <b>%d</b> answers%s.</li>",
                  sum(rare), if (length(rare_ex)) paste0(", like ", paste(rare_ex, collapse = ", ")) else "")
    trap <- if (!is.na(worst_i)) sprintf(
      "<li>The cruelest near-miss: <b>%s</b> is a single letter away from <b>%d</b> other answers in the original list, the classic one-blank coin flip. <b>%d</b> answers sat in a look-alike family of five or more.</li>",
      sol[worst_i], fam[worst_i] - 1L, sum(fam >= 5, na.rm = TRUE)) else ""
    paste0(
      '<section><div class="section-head"><span class="tag">Beyond the pattern</span>',
      '<h2>What makes a Wordle nasty</h2>',
      sprintf('<p class="sub">%s is one shape. These are the traits Wordle players grumble about, counted across the %s answers on record.</p></div>',
              esc(WORDLE_PATTERN), comma(length(sol))),
      '<ul class="facts">', reps, rr, trap, '</ul></section>')
  }

  # -- mini and midi -----------------------------------------------------------
  small <- vapply(c("mini", "midi"), function(g) {
    rows <- history[history$game == g, ]
    if (!nrow(rows)) return(sprintf("<li>%s: no data yet.</li>", names(CROSSWORDS)[CROSSWORDS == g]))
    sprintf("<li>%s: %d rebus puzzles out of %s, from %s to %s.</li>",
            names(CROSSWORDS)[CROSSWORDS == g], sum(rows$hit), comma(nrow(rows)),
            nice_date(min(rows$date)), nice_date(max(rows$date)))
  }, character(1))
  small_cw <- nrow(history[history$game %in% c("mini", "midi"), ])

  # -- day lookup: embed only the days with a rebus or a gimmick, plus range ---
  lk   <- daily[daily$rebus_cells > 0 | (!is.na(daily$gimmick_cells) & daily$gimmick_cells > 0), ]
  lk_g <- ifelse(is.na(lk$gimmick_cells), 0L, lk$gimmick_cells)
  lookup_json    <- paste0("{", paste(sprintf('"%s":[%d,%d]', lk$date, lk$rebus_cells, lk_g), collapse = ","), "}")
  daily_min      <- min(daily$date)
  daily_max      <- max(daily$date)
  lookup_default <- max(rebuses$date)
  data_script    <- sprintf('<script>window.RB_DATA={min:"%s",max:"%s",days:%s};</script>',
                            daily_min, daily_max, lookup_json)

  # -- difficulty calendar (from data/difficulty.csv, scraped from XW Stats) ----
  diff_df   <- if (exists("read_difficulty")) tryCatch(read_difficulty(), error = function(e) NULL) else NULL
  have_diff <- !is.null(diff_df) && nrow(diff_df) && any(!is.na(diff_df$vs_weekday))
  difficulty_panel <- if (have_diff) {
    dd   <- diff_df[!is.na(diff_df$vs_weekday), ]
    dmap <- new.env(parent = emptyenv())
    for (i in seq_len(nrow(dd))) assign(dd$date[i], list(vs = dd$vs_weekday[i], label = dd$difficulty[i]), envir = dmap)
    years    <- sort(unique(as.integer(substr(dd$date, 1, 4))), decreasing = TRUE)
    heatmaps <- paste(vapply(years, function(y) diff_year_heatmap(y, dmap), character(1)), collapse = "")
    n_hard   <- sum(dd$vs_weekday > 0); n_easy <- sum(dd$vs_weekday < 0)
    sw <- function(hue, pct) sprintf('<span class="sw" style="background:color-mix(in srgb,var(%s) %d%%,var(--surface-2))"></span>', hue, pct)
    paste0(
      '<div class="panel" id="panel-difficulty" role="tabpanel">',
      '<header class="whero"><h1 class="hero-title">How hard was each day, really?</h1>',
      sprintf('<p class="lede">Each square is a daily crossword, shaded by how the median solver did against their own average for that <b>weekday</b>: cooler is easier than a normal day, warmer is harder. %s days have a difficulty read so far.</p></header>',
              comma(nrow(dd))),
      '<section><div class="section-head"><h2>The difficulty calendar</h2>',
      '<p class="sub">Newest year first. Hover or focus a square for the day, its rating, and how far it ran from a typical puzzle on that weekday.</p></div>',
      '<div class="cal-legend"><span>Easier</span>', sw("--cool", 82), sw("--cool", 40),
      '<span class="sw" style="background:var(--surface-2)"></span>', sw("--accent", 40), sw("--accent", 82),
      '<span>Harder</span></div>',
      '<div class="cal-wrap">', heatmaps, '</div>',
      sprintf('<p class="cal-src">Difficulty from <a href="https://xwstats.com">XW Stats</a>, which aggregates community solve times. %s puzzles ran harder than their weekday norm, %s easier. Not every day has enough solves yet.</p>',
              comma(n_hard), comma(n_easy)),
      '</section></div>')
  } else ""

  # -- stat bands --------------------------------------------------------------
  rebus_stats <- paste0(
    stat(nrow(daily), "Daily crosswords analyzed",
         sprintf("%s to %s", format(as.Date(min(daily$date)), "%Y"), format(as.Date(max(daily$date)), "%Y"))),
    stat(nrow(rebuses), "Held a rebus", sprintf("about 1 in %d puzzles", one_in), accent = TRUE),
    if (have_g) stat(nrow(gim), "Alternate-answer gimmicks", "a different trick, tracked apart")
    else        stat(small_cw, "Mini and Midi scanned", "the shorter crosswords too"),
    stat(biggest$rebus_cells, "Most rebus squares in one", sprintf("on %s", nice_date(biggest$date))))

  wordle_stats <- paste0(
    stat(nrow(wordle), "Wordles checked",
         sprintf("since %s", format(as.Date(min(wordle$date)), "%Y"))),
    stat(n_either, "Fit either pattern", sprintf("about 1 in %d", either_one_in), accent = TRUE),
    paste(vapply(patterns, function(p) {
      i <- pat_info[[p]]
      stat(i$n, paste("Fit", p), sprintf("1 in %d", i$one_in))
    }, character(1)), collapse = ""))

  built    <- nice_date(Sys.Date())
  next_ref <- nice_date(first_of_next_month())

  head <- paste0(
    '<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1">',
    '<meta name="description" content="A running history of rebuses in the New York Times crossword and Wordle answers that fit a chosen pattern. Refreshed monthly.">',
    sprintf('<link rel="canonical" href="%s/">', SITE_URL),
    sprintf('<link rel="icon" href="%s">', FAVICON),
    sprintf('<link rel="apple-touch-icon" href="%s">', FAVICON),
    '<meta property="og:type" content="website">',
    sprintf('<meta property="og:url" content="%s/">', SITE_URL),
    '<meta property="og:title" content="Rebus &amp; Wordle History">',
    '<meta property="og:description" content="How often the NYT crossword hides a rebus (almost always a Thursday), and how often Wordle lands on __A_E. 32 years of data.">',
    sprintf('<meta property="og:image" content="%s/og.png">', SITE_URL),
    '<meta property="og:image:width" content="1200"><meta property="og:image:height" content="630">',
    '<meta name="twitter:card" content="summary_large_image">',
    '<meta name="twitter:title" content="Rebus &amp; Wordle History">',
    '<meta name="twitter:description" content="How often the NYT crossword hides a rebus, and how often Wordle lands on __A_E.">',
    sprintf('<meta name="twitter:image" content="%s/og.png">', SITE_URL),
    "<title>Rebus and Wordle History</title><style>", REPORT_CSS, "</style></head><body><main>")

  topbar <- paste0(
    '<div class="topbar"><div class="brand">', MARK, '<span>NYT Games history</span></div>',
    '<div class="tabs" role="tablist" aria-label="Choose a game">',
    '<button class="tab" role="tab" data-tab="rebus" aria-selected="true">Rebus</button>',
    '<button class="tab" role="tab" data-tab="wordle" aria-selected="false">Wordle</button>',
    if (have_diff) '<button class="tab" role="tab" data-tab="difficulty" aria-selected="false">Difficulty</button>' else "",
    '</div></div>')

  rebus_panel <- paste0(
    '<div class="panel" id="panel-rebus" role="tabpanel">',
    '<header class="hero">',
    '<h1 class="hero-title">Thursday is where the tricks hide.</h1>',
    sprintf('<p class="lede">For %d years the New York Times daily crossword has slipped in a <b>rebus</b>, a square holding more than one letter, about <b>1 in %d</b> puzzles. It happens far more on some days than others.</p>',
            span_years, one_in),
    '<div class="stats" data-reveal>', rebus_stats, "</div>",
    "</header>",

    '<section><div class="section-head"><h2>Was there a rebus on a given day?</h2>',
    sprintf('<p class="sub">Pick any date from %s to %s. The daily crossword is checked for a rebus (a square holding more than one letter) and for an alternate-answer gimmick.</p></div>',
            nice_date(daily_min), nice_date(daily_max)),
    sprintf('<div class="lookup"><input type="date" id="lk-date" min="%s" max="%s" value="%s" aria-label="Pick a date"><div id="lk-result" class="lk-result" role="status"></div></div></section>',
            daily_min, daily_max, lookup_default),

    '<section><div class="section-head"><h2>', esc(weekday_title), "</h2>",
    '<p class="sub">Share of daily crosswords with at least one rebus square, by day of the week. Puzzles get harder from Monday to Saturday; Sunday is larger and about Thursday difficulty.</p></div>',
    weekday_chart, table_view(weekday_table), "</section>",

    '<section><div class="section-head"><h2>Rebus puzzles per year</h2>',
    sprintf('<p class="sub">Count of daily crosswords with a rebus in each calendar year. %s and %s are partial years in this data.</p></div>',
            min(by_year$year), max(by_year$year)),
    year_chart, table_view(year_table), "</section>",

    '<section class="scrolly"><div class="section-head"><h2>The habit the NYT grew into</h2>',
    sprintf('<p class="sub">%s Each cell is a weekday\'s rebus share within one decade; darker means higher.</p></div>', esc(scrolly_dek)),
    '<div class="scrolly-grid"><div class="scrolly-viz">', decade_grid(daily), '</div>',
    '<div class="scrolly-steps">', steps, '</div></div></section>',

    '<section><div class="section-head"><h2>Rebus details</h2></div><ul class="facts">',
    sprintf("<li>Most recent rebus: <b>%s</b> (%s).</li>", nice_date(max(rebuses$date)),
            rebuses$weekday[which.max(as.Date(rebuses$date))]),
    sprintf("<li>Typical gap between rebus puzzles: <b>%.0f days</b> (median). Longest gap: %d days, ending %s.</li>",
            stats::median(as.numeric(gaps)), as.integer(max(gaps)), nice_date(rebuses$date[which.max(gaps) + 1])),
    sprintf("<li>Rebus squares in a rebus puzzle: <b>%.0f</b> is typical (median). The most is %d, on %s.</li>",
            stats::median(rebuses$rebus_cells), biggest$rebus_cells, nice_date(biggest$date)),
    paste(small, collapse = ""), "</ul></section>",

    gimmick_section,

    '<section><div class="section-head"><h2>Method and limits</h2></div><ul class="facts">',
    "<li>Source: the puzzle JSON that nytimes.com serves to its own game pages, read without a login. A rebus is any square whose stored answer is longer than one character.</li>",
    "<li>Cross-checked: a sample of rebus dates was confirmed against XWord Info's independent rebus list, and every stored value re-derived from the live puzzle on the last full scan.</li>",
    "<li>A gimmick square is a single letter cell that still accepts another answer. Ordinary puzzles have none; visual-only gimmicks leave no trace and are not counted.</li>",
    sprintf("<li>Coverage: %s of the %s calendar days from %s to %s have a daily crossword on record%s.</li>",
            comma(nrow(daily)), comma(expected), nice_date(min(daily$date)), nice_date(max(daily$date)),
            if (nrow(daily) < expected) sprintf(", so %s days are missing", comma(expected - nrow(daily))) else ""),
    sprintf("<li>Refreshed on the first of each month. Last built %s; next refresh %s.</li>", built, next_ref),
    "</ul></section>",
    "</div>")

  describe_pattern <- function(p) {
    ch <- strsplit(p, "")[[1]]; ord <- c("first", "second", "third", "fourth", "fifth")
    fixed <- which(ch != "_")
    and_list(sprintf("%s %s", ch[fixed], ord[fixed]))
  }
  pattern_or <- paste(patterns, collapse = " or ")
  hero_tiles <- paste(vapply(patterns, function(p) wordle_tiles(p, big = TRUE), character(1)), collapse = "")
  pat_sections <- paste(vapply(patterns, function(p) {
    i  <- pat_info[[p]]
    wl <- paste(sprintf("<li><b>%s</b><span>%s</span></li>", esc(rev(i$rows$solution)),
                        esc(rev(i$rows$date))), collapse = "")
    gapline <- if (length(i$gaps)) sprintf("<li>Typical gap between matches: <b>%.0f days</b> (median); longest %d days.</li>",
                                           stats::median(as.numeric(i$gaps)), as.integer(max(i$gaps))) else ""
    paste0(
      '<section><div class="section-head"><h2>', esc(p), '</h2>',
      sprintf('<p class="sub">%s. %d of %s answers on record fit (%s), about 1 in %d.</p></div>',
              describe_pattern(p), i$n, comma(nrow(w_known)), pct(i$rate), i$one_in),
      wordle_tiles(p),
      '<ul class="facts">',
      sprintf('<li>Most recent match: <b>%s</b>.</li>', nice_date(i$recent)),
      gapline, '</ul>',
      sprintf('<details><summary>The %d answers that fit %s, newest first</summary>', i$n, esc(p)),
      '<p class="note">Listed only after the day has passed, so nothing here spoils the current puzzle.</p>',
      '<ul class="words">', wl, '</ul></details></section>')
  }, character(1)), collapse = "")

  wordle_panel <- paste0(
    '<div class="panel" id="panel-wordle" role="tabpanel">',
    '<header class="whero">',
    hero_tiles,
    sprintf('<h1 class="hero-title">Does the answer land on %s?</h1>', esc(pattern_or)),
    sprintf('<p class="lede">Every Wordle answer is checked against %d %s. About <b>1 in %d</b> fit at least one.%s</p>',
            length(patterns), if (length(patterns) == 1) "pattern" else "patterns", either_one_in, esc(baseline)),
    '<div class="stats" data-reveal>', wordle_stats, "</div>",
    "</header>",

    '<section><div class="section-head"><h2>Matches per year</h2>',
    sprintf('<p class="sub">Answers fitting any tracked pattern each year, since the archive begins in %s. The table breaks it down by pattern.</p></div>',
            format(as.Date(min(wordle$date)), "%Y")),
    w_year_chart, table_view(w_year_table), "</section>",

    pat_sections,

    gripes_section,

    '<section><div class="section-head"><h2>Method and limits</h2></div><ul class="facts">',
    "<li>Source: the Wordle JSON that nytimes.com serves to the game, read without a login. Every match is derived from the stored answer, so adding a pattern needs no re-fetch.</li>",
    "<li>The gripe counts (repeats, rare letters, look-alike families) are computed from the answers on record; the family sizes use the original Wordle answer list.</li>",
    "<li>Cross-checked: stored answers were spot-checked against public Wordle archives, and 97% appear in the original answer word list (the rest are real words the NYT added later).</li>",
    sprintf("<li>The patterns live in one place, WORDLE_PATTERNS, currently %s.</li>", esc(and_list(patterns))),
    sprintf("<li>Refreshed on the first of each month. Last built %s; next refresh %s.</li>", built, next_ref),
    "</ul></section>",
    "</div>")

  footer <- paste0(
    '<footer><div class="foot-cols">',
    '<div><h4>Data sources</h4><ul>',
    '<li><a href="https://www.nytimes.com/crosswords">NYT Crossword puzzle data</a></li>',
    '<li><a href="https://www.nytimes.com/games/wordle/index.html">NYT Wordle</a></li>',
    '<li><a href="https://www.xwordinfo.com/Rebus">XWord Info rebus list</a> (validation)</li>',
    '</ul></div>',
    '<div><h4>Built by</h4><ul>',
    '<li><a href="https://chester.rbind.io">Chester Ismay</a></li>',
    '<li><a href="https://github.com/ismayc/nyt-games-alerter">Source on GitHub</a></li>',
    '<li>with <a href="https://claude.com/claude-code">Claude</a></li>',
    '</ul></div></div>',
    sprintf('<div class="foot-note"><span>Refreshed on the first of each month. Last built %s.</span><span>Answers are shown only after their day has passed.</span></div>', built),
    '</footer>')

  page <- paste0(head, topbar, rebus_panel, wordle_panel, difficulty_panel, footer,
                 '<div id="tip" role="status"><b></b><span></span></div>',
                 "</main>", data_script, "<script>", REPORT_JS, "</script></body></html>")

  if (!dir.exists(dirname(out))) dir.create(dirname(out), recursive = TRUE)
  tmp <- paste0(out, ".tmp")
  writeLines(page, tmp, useBytes = TRUE)
  file.rename(tmp, out)
  log_msg(sprintf("[report] wrote %s (%d bytes)", out, file.size(out)))
  invisible(out)
}
