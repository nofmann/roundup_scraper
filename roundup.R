#!/usr/bin/env Rscript
# Alta Hourly Wx Roundup: scrape -> append to CSV -> HTML report with table + plots.

suppressPackageStartupMessages({
  library(httr); library(dplyr); library(tidyr); library(readr)
  library(ggplot2); library(patchwork); library(scales)
  library(base64enc); library(ragg)
})

# ---- config ---------------------------------------------------------------
URL         <- "https://www.wxstns.net/ALTA.html"
TZ          <- "America/Denver"
PLOT_HOURS  <- as.integer(Sys.getenv("ROUNDUP_PLOT_HOURS", "72"))
TABLE_HOURS <- as.integer(Sys.getenv("ROUNDUP_TABLE_HOURS", "24"))

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a[1]))) else getwd()
})
CSV_PATH <- file.path(script_dir, "data", "alta_hourly.csv")
OUT_DIR  <- file.path(script_dir, "output")
dir.create(dirname(CSV_PATH), showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

COLS <- c("h2o_9664", "snow12_9664",
          "temp_8550", "temp_10500", "temp_11068",
          "rh_8550", "rh_10500", "rh_11068",
          "wspd_8550", "wdir_8550", "wgust_8550",
          "wspd_10500", "wdir_10500", "wgust_10500",
          "wspd_baldy", "wdir_baldy", "wgust_baldy",
          "h2o_8550")

# ---- scrape ---------------------------------------------------------------
fetch_roundup <- function() {
  r <- GET(URL, user_agent("roundup-scraper (R)"), timeout(30))
  stop_for_status(r)
  content(r, "text", encoding = "latin1")  # page has a stray degree symbol
}

parse_roundup <- function(txt) {
  lines <- strsplit(txt, "\r?\n")[[1]]
  lines <- lines[grepl("^\\s*\\d{2}/\\d{2}/\\d{4}\\s+\\d{2}:\\d{2}\\s", lines)]
  f <- strsplit(trimws(lines), "\\s+")
  ok <- lengths(f) == length(COLS) + 2
  if (any(!ok)) warning(sum(!ok), " row(s) skipped: unexpected field count")
  if (!any(ok)) stop("No data rows parsed; has the page format changed?")
  m <- do.call(rbind, f[ok])

  df <- as.data.frame(m[, -(1:2), drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- COLS
  df[] <- lapply(df, function(x) {
    x <- suppressWarnings(as.numeric(x))
    ifelse(x <= -7999, NA, x)  # logger sentinel
  })

  day <- as.Date(m[, 1], "%m/%d/%Y")
  hr  <- as.integer(substr(m[, 2], 1, 2))  # 24 rolls to 00:00 next day
  dt  <- as.POSIXct(format(day), tz = TZ) + hr * 3600
  bind_cols(tibble(datetime = dt), df)
}

# ---- append to CSV --------------------------------------------------------
load_history <- function() {
  if (!file.exists(CSV_PATH)) return(NULL)
  h <- read_csv(CSV_PATH, col_types = cols(.default = col_double(),
                                           datetime = col_character()),
                progress = FALSE)
  h$datetime <- as.POSIXct(h$datetime, tz = TZ, format = "%Y-%m-%d %H:%M:%S")
  h
}

save_history <- function(df) {
  out <- df
  out$datetime <- format(out$datetime, "%Y-%m-%d %H:%M:%S", tz = TZ)
  write_csv(out, CSV_PATH, na = "")
}

update_history <- function(new) {
  old <- load_history()
  n_before <- if (is.null(old)) 0L else nrow(old)
  all <- bind_rows(new, old) |>
    distinct(datetime, .keep_all = TRUE) |>   # new rows come first, so they win
    arrange(datetime)
  save_history(all)
  message(sprintf("Scraped %d rows; history %d -> %d rows (%s)",
                  nrow(new), n_before, nrow(all), CSV_PATH))
  all
}

# ---- plots ----------------------------------------------------------------
ELEV_COL <- c("8,550 ft" = "#EF6323", "10,500 ft" = "#00AE9B", "11,068 ft" = "#0032A0")
PRECIP_COL <- "#0032A0"; SNOW_COL <- "#EF6323"
INK <- "#222222"; MUTED <- "#555555"; GRID <- "#d9d9d9"
CAPTION <- "Source: Alta Hourly Wx Roundup (wxstns.net). Times are hour-ending, Mountain."

theme_roundup <- function() {
  theme_linedraw(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
          panel.border = element_rect(colour = INK, fill = NA, linewidth = 0.6),
          axis.ticks = element_line(colour = INK, linewidth = 0.4),
          plot.title = element_text(face = "bold", colour = INK, size = 10),
          plot.subtitle = element_text(colour = MUTED, size = 10),
          plot.caption = element_text(colour = MUTED, size = 8, hjust = 0),
          plot.title.position = "plot", plot.caption.position = "plot",
          axis.title.x = element_blank(),
          axis.title.y = element_text(colour = INK, size = 10),
          axis.text = element_text(colour = INK, size = 9),
          plot.margin = margin(2, 14, 2, 8),
          legend.position = "top", legend.justification = "left",
          legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, 0, 0),
          legend.title = element_blank(),
          legend.key = element_blank(),
          strip.text = element_text(face = "bold", hjust = 0))
}

time_scale <- function(df) {
  span <- as.numeric(diff(range(df$datetime)), units = "hours")
  brk <- if (span > 48) "12 hours" else if (span > 24) "6 hours" else "3 hours"
  scale_x_datetime(date_breaks = brk, date_labels = "%b %d\n%H:%M", timezone = TZ,
                   limits = range(df$datetime), oob = scales::oob_keep,
                   expand = expansion(mult = 0.01))
}

long_elev <- function(df, prefix) {
  df |>
    select(datetime, starts_with(prefix)) |>
    pivot_longer(-datetime, names_to = "elev", values_to = "value") |>
    mutate(elev = factor(c("8550" = "8,550 ft", "10500" = "10,500 ft",
                           "11068" = "11,068 ft")[sub(".*_", "", elev)],
                         levels = names(ELEV_COL)))
}

plot_temp <- function(df) {
  d <- long_elev(df, "temp_")
  ggplot(d, aes(datetime, value, colour = elev)) +
    geom_hline(yintercept = 32, linetype = "dashed", colour = MUTED) +
    annotate("text", x = min(d$datetime), y = 32, label = "32 °F", vjust = -0.6,
             hjust = 0, size = 3.2, colour = MUTED) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    scale_colour_manual(values = ELEV_COL) +
    time_scale(df) +
    labs(title = "Temperature by elevation (hourly average)", y = "Temp (°F)") +
    theme_roundup()
}

plot_rh <- function(df) {
  d <- long_elev(df, "rh_")
  ggplot(d, aes(datetime, value, colour = elev)) +
    geom_line(linewidth = 0.9, na.rm = TRUE) +
    scale_colour_manual(values = ELEV_COL) +
    scale_y_continuous(limits = c(NA, 100), labels = function(x) paste0(x, "%")) +
    time_scale(df) +
    labs(title = "Relative humidity by elevation (hourly average)", y = "RH") +
    theme_roundup()
}

plot_precip <- function(df) {
  wmax <- max(pretty(c(0, max(df$h2o_9664, 0.05, na.rm = TRUE))))
  smax <- max(pretty(c(0, max(df$snow12_9664, 2, na.rm = TRUE))))
  k <- smax / wmax  # snow inches per water inch on the secondary axis
  ggplot(df, aes(datetime)) +
    geom_col(aes(y = h2o_9664), fill = PRECIP_COL, width = 2400, na.rm = TRUE) +
    geom_line(aes(y = snow12_9664 / k), colour = SNOW_COL, linewidth = 1, na.rm = TRUE) +
    scale_y_continuous(limits = c(0, wmax), expand = expansion(mult = c(0, 0.02)),
                       sec.axis = sec_axis(~ . * k, name = "12-hr snow (in)")) +
    time_scale(df) +
    labs(title = "Precipitation at 9,664 ft: bars = hourly water equivalent (left), line = rolling 12-hr snow (right)",
         y = "Water eq. (in)") +
    theme_roundup() +
    theme(axis.title.y.left = element_text(colour = PRECIP_COL, face = "bold"),
          axis.title.y.right = element_text(colour = SNOW_COL, face = "bold"))
}

wind_station <- function(df, suffix, label, col) {
  d <- tibble(datetime = df$datetime,
              spd = df[[paste0("wspd_", suffix)]],
              gust = df[[paste0("wgust_", suffix)]],
              dir = df[[paste0("wdir_", suffix)]])
  if (!any(d$gust > 0 | d$spd > 0, na.rm = TRUE)) return(NULL)
  d$dir[!is.na(d$spd) & d$spd == 0] <- NA  # direction meaningless when calm
  ymax <- max(pretty(c(0, max(d$gust, d$spd, 5, na.rm = TRUE))))
  ggplot(d, aes(datetime)) +
    geom_line(aes(y = gust), colour = col, linewidth = 0.7, linetype = "dashed",
              na.rm = TRUE) +
    geom_line(aes(y = spd), colour = col, linewidth = 1.1, na.rm = TRUE) +
    geom_point(aes(y = dir / 360 * ymax), shape = 21, fill = "white", colour = INK,
               size = 1.8, stroke = 0.5, na.rm = TRUE) +
    scale_y_continuous(limits = c(0, ymax), expand = expansion(mult = c(0, 0.02)),
                       sec.axis = sec_axis(~ . / ymax * 360, name = "Direction",
                                           breaks = c(0, 90, 180, 270, 360),
                                           labels = c("N", "E", "S", "W", "N"))) +
    time_scale(df) +
    labs(title = paste0("Wind at ", label, ": solid = average, dashed = max gust, circles = direction (right)"),
         y = "Speed (mph)") +
    theme_roundup()
}

wind_panels <- function(df) {
  Filter(Negate(is.null),
         list(wind_station(df, "8550", "8,550 ft", ELEV_COL[["8,550 ft"]]),
              wind_station(df, "10500", "10,500 ft", ELEV_COL[["10,500 ft"]]),
              wind_station(df, "baldy", "Mt. Baldy", "#555555")))
}

# One tall figure: all panels stacked on a shared time axis (labels on the bottom only).
plot_all <- function(df) {
  panels <- c(list(plot_precip(df), plot_temp(df), plot_rh(df)), wind_panels(df))
  n <- length(panels)
  no_x <- theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  panels[-n] <- lapply(panels[-n], function(p) p + no_x)
  wrap_plots(panels, ncol = 1, guides = "collect") +
    plot_annotation(caption = CAPTION, theme = theme_roundup()) &
    theme(legend.position = "top")
}

chart_data <- function(df) {
  d <- as.list(df[, setdiff(names(df), "datetime")])
  d$t <- as.numeric(df$datetime) * 1000
  jsonlite::toJSON(d, na = "null", digits = NA)
}

save_plot <- function(p, name, width = 12, height = 10) {
  path <- file.path(OUT_DIR, paste0(name, ".png"))
  ggsave(path, p, device = ragg::agg_png, width = width, height = height,
         dpi = 150, bg = "white")
  path
}

# ---- table ----------------------------------------------------------------
compass <- function(deg) {
  pts <- c("N","NNE","NE","ENE","E","ESE","SE","SSE","S","SSW","SW","WSW","W","WNW","NW","NNW")
  ifelse(is.na(deg), "", pts[(round(deg / 22.5) %% 16) + 1])
}

ramp_cell <- function(x, lo, hi, cols, fmt) {
  ramp <- colorRamp(cols, space = "Lab")
  out <- character(length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) { out[i] <- '<td class="na">&ndash;</td>'; next }
    t <- min(max((x[i] - lo) / (hi - lo), 0), 1)
    rgbv <- round(ramp(t)[1, ])
    lum <- (0.299 * rgbv[1] + 0.587 * rgbv[2] + 0.114 * rgbv[3]) / 255
    out[i] <- sprintf('<td style="background:rgb(%d,%d,%d);color:%s">%s</td>',
                      rgbv[1], rgbv[2], rgbv[3], if (lum < 0.5) "#fff" else "#111",
                      sprintf(fmt, x[i]))
  }
  out
}

# Diverging about 32 F: 2..62 F maps blue -> white -> red, white at 32.
temp_cells   <- function(x) ramp_cell(x, 2, 62, c("#2f5d9e", "#f4f4f4", "#d1495b"), "%.0f")
rh_cells     <- function(x) ramp_cell(x, 40, 100, c("#ffffff", "#7fb3d5"), "%.0f")
wind_cells   <- function(x) ramp_cell(x, 0, 40, c("#ffffff", "#f6a04d", "#b8382c"), "%.0f")
precip_cells <- function(x) ramp_cell(x, 0, 0.15, c("#ffffff", "#2b7bba"), "%.2f")
snow_cells   <- function(x) ramp_cell(x, 0, 12, c("#ffffff", "#2b7bba"), "%.1f")

build_table <- function(df) {
  d <- df |> arrange(desc(datetime)) |> head(TABLE_HOURS)
  lab <- format(d$datetime - 1, "%a %b %d", tz = TZ)  # hour-ending: 00:00 belongs to the prior day
  tm  <- sub("^00:", "24:", format(d$datetime, "%H:%M", tz = TZ))
  newday <- c(TRUE, lab[-1] != lab[-length(lab)])

  dirc <- function(spd, deg) ifelse(is.na(deg) | is.na(spd) | spd == 0, '<td class="na">&ndash;</td>',
                                    sprintf("<td>%s <span class='deg'>%03d&deg;</span></td>",
                                            compass(deg), as.integer(round(deg))))
  cols <- list(
    sprintf("<td class='dt'>%s</td>", tm),
    precip_cells(d$h2o_9664), precip_cells(d$h2o_8550), snow_cells(d$snow12_9664),
    temp_cells(d$temp_8550), temp_cells(d$temp_10500), temp_cells(d$temp_11068),
    rh_cells(d$rh_8550), rh_cells(d$rh_10500), rh_cells(d$rh_11068),
    wind_cells(d$wspd_8550), wind_cells(d$wgust_8550), dirc(d$wspd_8550, d$wdir_8550),
    wind_cells(d$wspd_10500), wind_cells(d$wgust_10500), dirc(d$wspd_10500, d$wdir_10500),
    wind_cells(d$wspd_baldy), wind_cells(d$wgust_baldy), dirc(d$wspd_baldy, d$wdir_baldy)
  )
  rows <- vapply(seq_len(nrow(d)), function(i) {
    cells <- paste(vapply(cols, `[`, "", i), collapse = "")
    day <- if (newday[i]) sprintf("<tr class='day'><td colspan='19'>%s</td></tr>", lab[i]) else ""
    paste0(day, "<tr>", cells, "</tr>")
  }, "")

  hdr <- paste0(
    "<thead><tr><th rowspan='2'>Time</th>",
    "<th colspan='3'>Precip / snow (in)</th><th colspan='3'>Temp (&deg;F)</th>",
    "<th colspan='3'>RH (%)</th><th colspan='3'>Wind 8,550 ft (mph)</th>",
    "<th colspan='3'>Wind 10,500 ft (mph)</th><th colspan='3'>Mt. Baldy wind (mph)</th></tr>",
    "<tr><th>H2O 9,664</th><th>H2O 8,550</th><th>Snow 12h</th>",
    "<th>8,550</th><th>10,500</th><th>11,068</th>",
    "<th>8,550</th><th>10,500</th><th>11,068</th>",
    "<th>Avg</th><th>Gust</th><th>Dir</th><th>Avg</th><th>Gust</th><th>Dir</th>",
    "<th>Avg</th><th>Gust</th><th>Dir</th></tr></thead>")
  paste0("<table class='wx'>", hdr, "<tbody>", paste(rows, collapse = ""), "</tbody></table>")
}

# ---- report ---------------------------------------------------------------
img_tag <- function(path, alt) {
  sprintf("<img alt='%s' src='data:image/png;base64,%s'>", alt,
          base64encode(path))
}

fmt <- function(x, f = "%.0f", na = "&ndash;") ifelse(is.na(x), na, sprintf(f, x))

build_report <- function(hist, plots, win) {
  now <- hist |> slice_max(datetime, n = 1)
  last24 <- hist |> filter(datetime > max(datetime) - 24 * 3600)
  tile <- function(label, value, sub = "") sprintf(
    "<div class='tile'><div class='lbl'>%s</div><div class='val'>%s</div><div class='sub'>%s</div></div>",
    label, value, sub)
  tiles <- paste0(
    tile("Temp 8,550 ft", paste0(fmt(now$temp_8550), "&deg;F"),
         sprintf("10,500: %s&deg; &middot; 11,068: %s&deg;", fmt(now$temp_10500), fmt(now$temp_11068))),
    tile("Wind 10,500 ft", paste0(fmt(now$wspd_10500), " mph"),
         sprintf("gust %s &middot; %s", fmt(now$wgust_10500),
                 ifelse(is.na(now$wdir_10500) || now$wspd_10500 == 0, "calm", compass(now$wdir_10500)))),
    tile("Wind 8,550 ft", paste0(fmt(now$wspd_8550), " mph"),
         sprintf("gust %s &middot; %s", fmt(now$wgust_8550),
                 ifelse(is.na(now$wdir_8550) || now$wspd_8550 == 0, "calm", compass(now$wdir_8550)))),
    tile("12-hr snow", paste0(fmt(now$snow12_9664, "%.1f"), " in"), "9,664 ft"),
    tile("24-hr water", paste0(fmt(sum(last24$h2o_9664, na.rm = TRUE), "%.2f"), " in"),
         sprintf("9,664 ft &middot; %d hrs of data", nrow(last24))),
    tile("RH 10,500 ft", paste0(fmt(now$rh_10500), "%"), "hourly average")
  )
  imgs <- paste0("<section class='plots'><div id='interactive'></div>",
                 "<noscript>", img_tag(plots[[1]], names(plots)[1]), "</noscript>",
                 "<script>window.ROUNDUP_DATA=", chart_data(win), ";</script>",
                 "<script>", paste(readLines(file.path(script_dir, "chart.js"), encoding = "UTF-8"),
                                   collapse = "\n"), "</script></section>")

  css <- "
  body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#222;margin:0;background:#f7f8fa}
  main{max-width:1280px;margin:0 auto;padding:20px 16px 40px}
  h1{margin:0 0 2px;font-size:24px} .asof{color:#6b7280;margin-bottom:16px}
  h2{font-size:16px;margin:28px 0 8px}
  .tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:10px}
  .tile{background:#fff;border:1px solid #e5e7eb;border-radius:8px;padding:10px 12px}
  .lbl{font-size:12px;color:#6b7280;text-transform:uppercase;letter-spacing:.04em}
  .val{font-size:26px;font-weight:650;margin:2px 0}.sub{font-size:12px;color:#6b7280}
  .scroll{overflow-x:auto;background:#fff;border:1px solid #e5e7eb;border-radius:8px}
  table.wx{border-collapse:collapse;font-size:12px;width:100%;font-variant-numeric:tabular-nums}
  .wx th{background:#1f2937;color:#fff;padding:4px 6px;font-weight:600;border-left:1px solid #374151;position:sticky;top:0}
  .wx td{padding:3px 6px;text-align:center;border-bottom:1px solid #fff;border-left:1px solid #fff;white-space:nowrap}
  .wx td.dt{background:#f3f4f6;font-weight:600}.wx td.na{color:#bbb}
  .wx tr.day td{background:#e5e7eb;text-align:left;font-weight:700;padding:4px 8px}
  .deg{color:#6b7280;font-size:10px}
  section.plots{background:#fff;border:1px solid #e5e7eb;border-radius:8px;padding:12px 16px;margin-top:28px}
  #interactive{position:relative}
  svg.ichart{display:block;width:100%;height:auto;max-height:calc(100vh - 56px);touch-action:pan-y;font-family:inherit}
  .itip{display:none;position:absolute;pointer-events:none;background:rgba(255,255,255,.96);border:1px solid #9ca3af;border-radius:6px;padding:6px 9px;font-size:12px;box-shadow:0 2px 8px rgba(0,0,0,.15);z-index:5}
  .itip .tt{font-weight:700;margin-bottom:2px}.itip .tt span{font-weight:400;color:#6b7280}
  .itip table{border-collapse:collapse}.itip td{padding:0 0 0 10px;white-space:nowrap}.itip td:first-child{padding-left:0;color:#374151}
  .itip td:last-child{text-align:right;font-variant-numeric:tabular-nums;font-weight:600}
  .itip th{text-align:left;font-size:11px;color:#6b7280;padding-top:4px;text-transform:uppercase;letter-spacing:.03em}
  .itip i{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:5px}
  noscript img{display:block;max-width:100%;margin:0 auto}"

  html <- sprintf("<!doctype html><html lang='en'><head><meta charset='utf-8'>
<meta name='viewport' content='width=device-width,initial-scale=1'>
<title>Alta Hourly Wx Roundup</title><style>%s</style></head><body><main>
<h1>Alta Hourly Wx Roundup</h1>
<div class='asof'>Latest observation: %s (hour ending) &middot; generated %s &middot; source: <a href='%s'>wxstns.net</a></div>
<div class='tiles'>%s</div>
<h2>Last %d hours</h2><div class='scroll'>%s</div>
%s
</main></body></html>",
    css, paste0(format(now$datetime - 1, "%a %b %d, ", tz = TZ), sub("^00:", "24:", format(now$datetime, "%H:%M %Z", tz = TZ))),
    format(Sys.time(), "%b %d %H:%M %Z", tz = TZ), URL, tiles,
    TABLE_HOURS, build_table(hist), imgs)
  path <- file.path(OUT_DIR, "roundup.html")
  writeLines(html, path, useBytes = TRUE)
  path
}

# ---- main -----------------------------------------------------------------
main <- function() {
  hist <- update_history(parse_roundup(fetch_roundup()))
  win <- hist |> filter(datetime > max(datetime) - PLOT_HOURS * 3600)

  plots <- list("Weather plots" = save_plot(plot_all(win), "roundup_plots"))
  out <- build_report(hist, plots, win)
  message("Report: ", out)
  invisible(out)
}

if (sys.nframe() == 0) main()
