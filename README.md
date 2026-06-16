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
├── CLAUDE.md              # standing rules for the repo
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
│   └── 05_tables_figures.R# tables (HTML/LaTeX) + figure
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

## Status
Cleaned FX and shock data, merged estimation panel, and all baseline, pooled,
fixed-effects, and heterogeneity models are in place. Preliminary tables and a
coefficient figure are generated. Rough draft under revision.

## Author
Haley J. Fennyery — Erasmus Mundus Joint Master's Degree, Economics of
Globalization and European Integration.

## Supervisor
Dr. Peter Claeys.

## Primary shock source
Based on the `china_mpshocks` framework (high-frequency Chinese monetary policy
surprises); see `docs/source_log.md`.
