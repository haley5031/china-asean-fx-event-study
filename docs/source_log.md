# Source Log

## Chinese monetary policy shocks
- File: `data-raw/policy/china_mpshocks.csv`
- Source repository: https://github.com/wtsong/china_mpshocks
- Raw file URL: https://raw.githubusercontent.com/wtsong/china_mpshocks/main/china_mpshocks.csv
- Local copy for this thesis (ASEAN-5 exchange-rate responses, 2006--2020).
- License: MIT (Copyright (c) 2022 Wenting Song). Redistribution is permitted
  provided the copyright and license notice are included; the full license is
  reproduced in `data-raw/policy/LICENSE_china_mpshocks.txt`. This file may be
  committed to the public repository.

## Foreign exchange
- File: `data-raw/fx/fx_asean5_filled.xlsx`
- Source: IMF representative exchange rates (local currency per USD).
- Coverage: 2006-01-01 to 2020-05-31.
- Adjustment: Philippine peso backfilled from Bangko Sentral ng Pilipinas (BSP)
  for 2006--2010; see the `PHP_fill_log` sheet.
- Provenance files retained: `Exchange_Rate_Report_IMF_ASEAN5.xls`,
  `pesodollar.xlsx`.

### NOTE: raw FX data IS committed, under IMF data-redistribution terms
The IMF "Use of IMF Data" terms (effective 2024-10-11) permit downloading,
creating derivative works from, and redistributing published IMF statistical
data — including Exchange Rate Data — provided the IMF is accurately credited
as the source and any material transformation is disclosed. The PHP backfill
from BSP is such a transformation and is disclosed in
`data-raw/fx/SOURCE_AND_LICENSE.md`, which also carries the IMF source
citation. On that basis the raw FX workbook is kept under version control.

Required source citation: International Monetary Fund, Exchange Rate Data,
https://www.imf.org/external/data.htm#exchange

## FOMC meeting dates
- File: `data-raw/external/fomc_meeting_dates_2008_2020.csv`
- Source: Federal Reserve Board FOMC meeting calendars and historical minutes
  (https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm and the
  year-by-year `fomchistorical*.htm` archive pages), plus FRASER
  (https://fraser.stlouisfed.org/) for 2020. Columns: `meeting_start`,
  `meeting_end`, `decision_date`, `meeting_type` (`scheduled`/`unscheduled`),
  `source` (`FED_HIST` / `FED_SCHED` / `FRASER`), `notes`.
- 103 scheduled + 5 unscheduled meetings, 2008-2020. 2020 has 7 scheduled
  meetings by design (the 17-18 March 2020 meeting was cancelled and
  superseded by the unscheduled 15 March meeting), not a data gap.
- This is a verified replacement for an earlier version of this file that was
  transcribed from memory of the published schedule rather than checked
  against the primary source, and that had real date errors as a result
  (see git history on this branch for the superseded file and the earlier,
  incorrect adjacency count it produced in R/18). The current file has been
  checked against the sources above, including spot-checks flagged in its own
  `notes` column where the tentative and final Fed calendars differed. Treat
  it as authoritative; do not regenerate it from the shock series or from
  memory.

## Licensing note
- Policy shocks: MIT (redistribution permitted; notice included).
- FX data: IMF Data terms (redistribution permitted with attribution +
  disclosure of the BSP transformation; both provided).
Reuse remains non-commercial; IMF data is free of charge and not for resale.
