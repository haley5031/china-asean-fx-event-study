# Chinese Monetary Policy Shocks and ASEAN-5 Exchange Rates: A Daily Event Study

Code, cleaned-data workflow, and draft materials for my master's thesis on
whether Chinese monetary policy shocks generate measurable short-run effects in
ASEAN-5 (Philippines, Malaysia, Thailand, Indonesia, Singapore) exchange rates.

## Research question
Do Chinese monetary policy shocks generate measurable short-run effects in
ASEAN-5 exchange rates?

## Design
Daily event study. Main sample 2008--2020 (full data 2006--2020). Dependent
variable: daily FX log return, local currency per USD. Preferred window
`[0,+1]`; benchmark `[0]`. Main shock `shock_1y`; robustness `shock_5y`.
Model progression: country OLS → pooled OLS → country fixed effects →
fixed-effects heterogeneity (interactions, Indonesia as reference).

## Repository layout
```
.
├── run_all.R              # rebuild everything, top to bottom
├── *.Rproj
├── data-raw/              # source files — never written to by scripts
│   ├── policy/
│   └── fx/
├── data-clean/            # produced only by scripts
├── R/
│   ├── 00_setup.R         # packages, paths, constants
│   ├── 01_load_policy.R   # raw shocks -> main + broad shock files
│   ├── 02_clean_fx.R      # raw FX workbook -> clean levels + returns
│   ├── 03_build_panel.R   # reshape, merge shocks, event windows
│   ├── 04_estimate.R      # all models -> output/models.rds
│   ├── 05_tables_figures.R# regression tables (HTML/LaTeX)
│   ├── 06_window_robustness.R # [0]/[0,+1]/[-1,+1] ladder, 1Y + 5Y shocks
│   ├── fig_theme.R        # shared font/palette/dims for every figure
│   ├── fig_fx_series.R    # Fig 1: ASEAN-5 indexed FX series
│   ├── fig_shock_stem.R   # Fig 2: Chinese MP surprise stem plot
│   ├── fig_cumulative_response.R # Fig 3: pooled cumulative FX response
│   ├── fig_attrition_funnel.R    # sample-construction attrition funnel
│   ├── fig_forest_country.R      # Fig 3a: country-level forest plot
│   ├── fig_forest_heterogeneity.R# Fig 3b: heterogeneity forest plot
│   └── 07_event_subset.R  # exploratory: FE re-estimated on event dates only
├── output/
│   ├── tables/
│   └── figures/
└── docs/
    ├── data_dictionary.md
    └──source_log.md
```

## How to run
1. Open the project via the `.Rproj` file (sets the working directory).
2. Install packages once (listed at the top of `R/00_setup.R`).
3. `source("run_all.R")` to rebuild `data-clean/` and `output/` from scratch.

Scripts can also be run individually in numbered order; each sources
`00_setup.R` first.

## Known limitations (researcher judgment, not coding errors)
- **Unmatched shock dates.** 31 of 102 main shock dates (about 30%) fall on a
  day with no FX observation (non-trading day) and are simply dropped by the
  join in `03_build_panel.R` -- they are not shifted to the nearest trading
  day. This is a defensible design choice but means some announcements never
  enter the estimation sample.
- **Overlapping `[0,+1]` windows.** A few main-sample shock dates are one
  trading day apart (e.g. 2012-07-03/2012-07-05 and 2015-08-26 and its
  successor), so one event's `+1` day is the next event's `0` day. The
  `fx_return_01` outcome for the earlier event therefore partly reflects the
  later shock. This affects a small number of observations and is a standard
  event-study trade-off, not a bug; it is not corrected automatically because
  doing so (e.g. dropping or truncating one of the two windows) is a
  methodological choice for the researcher to make explicitly.
- **Country-specific FX holidays.** The FX panel carries `NA` returns (not
  dropped rows) on days a given country's market was closed but at least one
  other ASEAN-5 market traded, so per-country sample sizes in `country_ols`
  differ slightly; this is intentional and handled by `lm()`/`feols()`
  list-wise deletion.

## Status
Cleaned FX and shock data, merged estimation panel, and all baseline, pooled,
fixed-effects, and heterogeneity models are in place. Tables, the full figure
set (indexed FX series, shock stem plot, cumulative response, attrition
funnel, and two forest plots), and an exploratory event-subset re-estimation
(`07_event_subset.R`, not yet wired into the thesis) are generated. Rough
draft under revision.

## Author
Haley J. Fennyery — Erasmus Mundus Joint Master's Degree, Economics of
Globalization and European Integration.

## Supervisor
Dr. Peter Claeys.

## Primary shock source
Based on the `china_mpshocks` framework (high-frequency Chinese monetary policy
surprises); see `docs/source_log.md`.
