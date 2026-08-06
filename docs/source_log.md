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
- Intended source: Federal Reserve Board, FOMC meeting calendars and historical
  minutes (https://www.federalreserve.gov/monetarypolicy/fomccalendars.htm and
  the year-by-year `fomchistorical*.htm` archive pages), which is the standard
  citation for this list.
- Compilation method: `federalreserve.gov` is not reachable from this project's
  execution environment (outbound access is restricted to a small domain
  allowlist that does not include it, confirmed by direct request), so the
  dates were NOT downloaded programmatically the way the FRED series in R/09
  are. They were transcribed from the standard published schedule (8 regularly
  scheduled meetings per year, decision/last day of each meeting) plus the
  well-documented unscheduled/emergency actions of 2008 (Jan 22, Oct 8) and
  2020 (Mar 3, Mar 15).
- ACTION NEEDED before this goes into the thesis: cross-check every date in
  this file against the Fed's official calendar. It has not been verified
  against the primary source and should be treated as provisional. If R/18
  is re-run in an environment with access to federalreserve.gov, replace this
  file with a programmatically fetched version and drop this caveat.

## Licensing note
- Policy shocks: MIT (redistribution permitted; notice included).
- FX data: IMF Data terms (redistribution permitted with attribution +
  disclosure of the BSP transformation; both provided).
Reuse remains non-commercial; IMF data is free of charge and not for resale.
