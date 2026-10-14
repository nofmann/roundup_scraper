# roundup-scraper

Scrapes the Alta Hourly Wx Roundup (<https://www.wxstns.net/ALTA.html>), appends new
hours to a CSV, and builds a self-contained HTML report (colour-scaled table + plots).

The source page only keeps the last 24 hours; the CSV in `data/` accumulates history.

## Run

```
Rscript roundup.R
```

Outputs:

- `data/alta_hourly.csv` – accumulated history (deduplicated on timestamp, newest scrape wins)
- `output/roundup.html` – the report (images embedded; open or email as a single file)
- `output/roundup_plots.png` – static fallback of the plots (the report itself has interactive hover plots, drawn by `chart.js`)

Options (environment variables): `ROUNDUP_PLOT_HOURS` (default 72, hours of history plotted),
`ROUNDUP_TABLE_HOURS` (default 24).

Required R packages: httr, dplyr, tidyr, readr, ggplot2, patchwork, scales, base64enc, ragg.

## Scheduling (Windows)

Task Scheduler → action `"C:\Program Files\R\R-4.5.2\bin\Rscript.exe"`, argument
`roundup.R`, start in this folder, trigger hourly.

## Notes

- Timestamps are hour-ending, America/Denver. The source's `24:00` is stored as 00:00 next day.
- Elevations in ft: 8,550 / 10,500 / 11,068. Snow is 12-hr total (in); water is 1-hr (in).
- Sentinel values (<= -7999) are stored as NA.
