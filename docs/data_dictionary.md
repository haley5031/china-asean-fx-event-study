# Data Dictionary

## Raw inputs (`data-raw/`, never modified by scripts)

### `data-raw/policy/china_mpshocks.csv`
Published Chinese monetary policy shock series. Key columns:
- `date` — announcement date
- `datetime` — announcement timestamp (no seconds; parse with `ymd_hm`)
- `shock_1y` — one-year swap shock (main measure)
- `shock_5y` — five-year swap shock (robustness measure)
- `isMain` — flag for the main event set
- `isBroad` — flag for the broader event set
- instrument flags: `isdRRR`, `isdRevrepo`, `isdLDR`, `isdMLF`, `isMPR`,
  `isFX`, `isTMLF`

### `data-raw/fx/fx_asean5_filled.xlsx`
IMF representative exchange rates, local currency per USD, 2006-01 to 2020-05,
with Philippine peso backfilled from BSP for 2006--2010. Sheet
`MAIN_ASEAN_EXCHANGE_RATES` has a title row above the header (read `skip = 1`).
Supporting sheets `PHP_fill_log` and `PHP_unfilled_2006_2010` document the backfill.
Other raw FX files (`Exchange_Rate_Report_IMF_ASEAN5.xls`, `pesodollar.xlsx`)
are retained as provenance for that workbook.

## Cleaned files (`data-clean/`, produced only by scripts)
- `policy_shocks_main.csv` — `isMain` shocks  (from 01)
- `policy_shocks_broad.csv` — `isBroad` shocks (from 01)
- `fx_asean5_clean.csv` — tidy FX levels, columns `date, idr, myr, php, sgd, thb` (from 02)
- `fx_returns_wide.csv` — levels plus `ret_*` daily log returns (from 02)
- `fx_panel.csv` — long country-date panel: `date, country, fx_lcu_per_usd, fx_return` (from 03)
- `reg_data0.csv` — panel merged with shocks, same-day `[0]` variables (from 03)
- `reg_data01_main.csv` — adds `fx_return_01` (`[0,+1]` outcome), restricted to 2008--2020 (from 03)

## Outputs (`output/`)
- `models.rds` — fitted model objects (from 04)
- `tables/` — regression tables, HTML + LaTeX (from 05 and 06)
- `tables/window_robustness_fe_shock_1y.{html,tex}`,
  `tables/window_robustness_fe_shock_5y.{html,tex}` — FE estimates across the
  `[0]`, `[0,+1]`, `[-1,+1]` windows for each shock measure (from 06)
- `tables/event_subset_comparison.csv` — baseline vs. event-subset FE
  coefficient/SE comparison (from 07, exploratory only)
- `figures/fig1_fx_indexed.{pdf,png}` — ASEAN-5 FX indexed to 100 at the
  first 2008 trading day (from `fig_fx_series.R`)
- `figures/fig2_shock_stem.{pdf,png}` — Chinese MP surprise stem plot,
  main event set, basis points (from `fig_shock_stem.R`)
- `figures/fig3_cumulative_response.{pdf,png}` — pooled average cumulative
  ASEAN-5 FX response with 95% CI band (from `fig_cumulative_response.R`)
- `figures/fig_attrition_funnel.{pdf,png}` — sample-construction attrition
  funnel, 102 -> 71 -> 65 -> 56 (from `fig_attrition_funnel.R`)
- `figures/fig3a_forest_country.{pdf,png}` — Table 1 country-level betas
  with 95% CIs (from `fig_forest_country.R`)
- `figures/fig3b_forest_heterogeneity.{pdf,png}` — Table 3 heterogeneity
  interactions with 95% CIs (from `fig_forest_heterogeneity.R`)
