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

## Licensing note
- Policy shocks: MIT (redistribution permitted; notice included).
- FX data: IMF Data terms (redistribution permitted with attribution +
  disclosure of the BSP transformation; both provided).
Reuse remains non-commercial; IMF data is free of charge and not for resale.
