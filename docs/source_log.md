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
- Source: the Federal Reserve's published FOMC calendars -- the 2008
  historical-materials page for the 2008 meetings (including the three
  unscheduled conference calls), the 2009-2019 schedule/meeting-date
  announcement press releases, and FRASER (https://fraser.stlouisfed.org/)
  for the 2020 meetings. Columns: `meeting_start`, `meeting_end`,
  `decision_date`, `meeting_type` (`scheduled`/`unscheduled`), `source`
  (per-row source tag), `notes`.
- 103 scheduled + 5 unscheduled meetings, 2008-2020. 2020 has 7 scheduled
  meetings by design (the 17-18 March 2020 meeting was cancelled and
  superseded by the unscheduled 15 March meeting), not a data gap.
- Full sourcing detail, including the per-row spot-checks against the
  tentative vs. final Fed calendar: see
  `docs/fomc_dates_verification_note.md`.
- This is a verified replacement for an earlier version of this file that was
  transcribed from memory of the published schedule rather than checked
  against the primary source, and that had real date errors as a result
  (see git history on this branch for the superseded file and the earlier,
  incorrect adjacency count it produced in R/18). Treat the current file as
  authoritative; do not regenerate it from the shock series or from memory.

## External daily series (FRED)
Pulled by `R/09_external_data.R` (five series) and `R/14_dollar_control_fix.R`
(one series) via the public FRED `fredgraph.csv` endpoint (no API key
required). Each series is cached on first fetch to
`data-raw/external/<SERIES_ID>.csv` and reused on later runs, so the pipeline
is reproducible offline once those files exist; all six are currently cached.
Coverage requested at fetch time: 2005-01-01 to 2020-12-31 (`FRED_START`/
`FRED_END` in `R/09_external_data.R`), trimmed to the 2008-2020 estimation
sample (`SAMPLE_START`/`SAMPLE_END`) downstream.

| Series ID | Measures | Original source (via FRED) | Used for |
|---|---|---|---|
| `DEXCHUS` | China/U.S. FX rate, CNY per USD, daily | Federal Reserve Board of Governors | RMB numeraire construction (`R/10`, `R/11`) |
| `VIXCLS` | CBOE Volatility Index (VIX), daily | Chicago Board Options Exchange | Global risk control (`R/13`) |
| `DTWEXBGS` | Nominal broad U.S. dollar index, daily | Federal Reserve Board of Governors | Dollar-leg control; `R/14` documents that it contains the RMB and all five ASEAN-5 currencies, so it is retained for contrast but not used as the preferred control |
| `DGS2` | 2-year Treasury constant-maturity yield, daily | Federal Reserve Board of Governors | U.S. monetary-policy control (`R/13`) |
| `DCOILBRENTEU` | Brent crude, USD/barrel, daily | U.S. Energy Information Administration | Commodity control (`R/13`) |
| `DTWEXAFEGS` | Nominal advanced-foreign-economies U.S. dollar index, daily | Federal Reserve Board of Governors | Preferred dollar-factor mediator/control (`R/14`, `R/15`, `R/19`): excludes China and all ASEAN-5 currencies, so unlike `DTWEXBGS` it does not mechanically contain the transmission channel or the outcome |

FRED terms of use (https://fred.stlouisfed.org/legal/): this project's
execution environment could not reach fred.stlouisfed.org to re-fetch or
re-verify the live terms page (the same network restriction noted for the
FOMC calendar above), so the following is stated from general knowledge of
FRED's stated policy and should be checked against the live page before
submission. FRED data is provided for free use with a request to cite FRED
as the source. `DEXCHUS`, `DTWEXBGS`, `DTWEXAFEGS`, and `DGS2` originate with
the Federal Reserve Board of Governors and `DCOILBRENTEU` with the U.S.
Energy Information Administration -- both U.S. government sources. `VIXCLS`
originates with the Chicago Board Options Exchange (CBOE), a private-sector
source redistributed via FRED, which may carry different terms from the
government-sourced series; check CBOE's own data-use terms separately if
`VIXCLS` figures are reproduced outside this repository.

Required source citation: Federal Reserve Bank of St. Louis, FRED,
https://fred.stlouisfed.org/, plus the original-source attribution in the
table above for each series.

## Licensing note
- Policy shocks: MIT (redistribution permitted; notice included).
- FX data: IMF Data terms (redistribution permitted with attribution +
  disclosure of the BSP transformation; both provided).
Reuse remains non-commercial; IMF data is free of charge and not for resale.
