# Chinese Monetary Policy Shocks and ASEAN-5 Exchange Rates: A Daily Event Study

This repository holds the data-processing and estimation code for a master's
thesis. This file is written for a reader who will look at the R scripts
alongside the paper but does not work in R day to day.

## What the project estimates

The thesis asks whether high-frequency Chinese monetary policy surprises move
ASEAN-5 exchange rates in the days around the announcement. The shock measure
is the swap-market-based series from Das and Song (the codebase's own
comments cite this as both "2022" and "2023" in different places -- check
which is the correct publication year for the bibliography before submission;
this has not been reconciled). The design is a daily event study: for each
Chinese monetary policy announcement date, the daily log return of each
ASEAN-5 currency against the U.S. dollar (Indonesian rupiah, Malaysian
ringgit, Philippine peso, Singapore dollar, Thai baht) is regressed on the
size of the surprise, with country fixed effects and standard errors
clustered by announcement date. The main sample runs 2008-2020, the preferred
event window is the announcement day plus the following day (`[0,+1]`), and
results are reported for a one-year-tenor shock measure (`shock_1y`) with a
five-year measure (`shock_5y`) as a robustness check throughout.

## Data sources

Full provenance, licensing, and citation requirements for every input are in
`docs/source_log.md` -- this section only summarizes what is constructed
in-house versus taken as published.

**Taken as published, used as-is:** the Chinese monetary policy shock series
(`data-raw/policy/china_mpshocks.csv`, from the published `china_mpshocks`
repository); the IMF representative exchange rates
(`data-raw/fx/fx_asean5_filled.xlsx`); six FRED daily series (the RMB/USD
rate, VIX, two U.S. dollar indices, the 2-year Treasury yield, and Brent
crude); and a verified list of FOMC meeting dates
(`data-raw/external/fomc_meeting_dates_2008_2020.csv`; see
`docs/fomc_dates_verification_note.md` for how it was checked).

**Constructed for this project:** cleaning and reshaping the raw FX workbook
into a tidy daily panel; merging the shock series onto FX trading dates;
building the `[0]`, `[0,+1]`, and `[-1,+1]` event-window outcomes; the
extended panel with an RMB numeraire, a forward-rolled (weekend/holiday
corrected) version of the shock dates, and the external control series merged
in on the FX trading calendar; and every regression, robustness check, and
table in `output/`.

## How to reproduce

Install R (developed and tested against R 4.3), then install the packages
listed at the top of `R/00_setup.R`:
`readr, readxl, dplyr, tidyr, lubridate, fixest, modelsummary, tinytable,
ggplot2`. Open the project via the `.Rproj` file so the working directory is
set correctly, then run:

```r
source("run_all.R")
```

This rebuilds `data-clean/` and `output/` from the files in `data-raw/`.
`run_all.R` now runs every script 01-20 (R/11-17 were wired in later and are
no longer separate from this list -- see below). In a verification run of
the current branch, the full 01-20 pipeline completed in 3-6 minutes with no
errors across two clean-state runs (most of the variation, and most of the
time either way, is one script, `R/19`, which runs a 2,000-replication
bootstrap; treat "a few minutes" as the realistic expectation rather than
either single figure). It downloads nothing on that run if the FRED files
already sitting in `data-raw/external/` are left in place -- delete that
folder only if you want to force a fresh download, which needs internet
access to fred.stlouisfed.org.

Every script can also be run individually, in numbered order; each starts
with `source("R/00_setup.R")`, so each is self-contained given the files
upstream of it in `data-clean/`.

## Script-to-exhibit map

The "paper exhibit" column below is the label used *inside this codebase* --
script comments, console messages, and `docs/data_dictionary.md` -- not a
citation I have cross-checked against the current Overleaf draft. Confirm the
numbering still matches before citing this table to anyone. Every script
01-20 is now called by `run_all.R`, in dependency order rather than numeric
order where the two differ (they don't, here -- see the comment above R/09
in `run_all.R` for how each script's actual reads/writes were used to derive
the order).

| Script | What it does | Output | Paper exhibit (as labeled in-code) |
|---|---|---|---|
| `00_setup.R` | Packages, paths, shared constants (sample dates, country list, significance-star convention). Sourced by every other script, never run on its own. | — | — |
| `01_load_policy.R` | Splits the raw shock file into the "main" and "broader" event sets. | `data-clean/policy_shocks_main.csv`, `policy_shocks_broad.csv` | — |
| `02_clean_fx.R` | Cleans the raw FX workbook into tidy levels and daily log returns. | `data-clean/fx_asean5_clean.csv`, `fx_returns_wide.csv` | — |
| `03_build_panel.R` | Reshapes to a long country-date panel, merges shocks onto FX dates by **exact date match**, builds the `[0,+1]` outcome, restricts to 2008-2020. | `data-clean/fx_panel.csv`, `reg_data0.csv`, `reg_data01_main.csv` (the main estimation panel) | — |
| `04_estimate.R` | Fits every baseline model: per-country OLS, pooled OLS, fixed-effects, fixed-effects with country interactions. | `output/models.rds` | feeds Tables 1-3 and the forest-plot figures |
| `05_tables_figures.R` | Builds the three headline regression tables from `models.rds`. | `output/tables/country_results_table.*`, `panel_results_table.*`, `heterogeneity_results_table.*` | Table 1 (country-level), Table 2 (pooled/FE), Table 3 (heterogeneity) |
| `06_window_robustness.R` | Re-estimates the FE and heterogeneity models across `[0]`, `[0,+1]`, `[-1,+1]` for both shock measures. | `output/tables/window_robustness_fe_shock_1y.*`, `_5y.*` | window-robustness appendix table |
| `fig_fx_series.R` | ASEAN-5 FX levels indexed to 100 at the start of 2008. | `output/figures/fig1_fx_indexed.*` | Figure 1 |
| `fig_shock_stem.R` | Stem plot of the Chinese MP surprise series. | `fig2_shock_stem.*` | Figure 2 |
| `fig_cumulative_response.R` | Pooled average cumulative FX response with a 95% band. | `fig3_cumulative_response.*` | Figure 3 |
| `fig_attrition_funnel.R` | How the sample shrinks: 102 raw event dates -> 71 matched to a trading day -> 65 in 2008-2020 -> 56 with a nonzero one-year surprise. | `fig_attrition_funnel.*` | attrition/sample-construction figure |
| `fig_forest_country.R` | Table 1's country-level coefficients with 95% CIs. | `fig3a_forest_country.*` | Figure 3a |
| `fig_forest_heterogeneity.R` | Table 3's country-interaction coefficients with 95% CIs. | `fig3b_forest_heterogeneity.*` | Figure 3b |
| `07_event_subset.R` | Exploratory: the FE spec re-estimated on only the 56 nonzero-surprise dates instead of the full panel. Explicitly not wired into any thesis table. | `output/tables/event_subset_comparison.csv` | none -- exploratory only |
| `08_descriptive_stats.R` | Summary statistics for FX returns and the two shock measures over the estimation sample. | `output/tables/descriptive_stats_table.tex` | descriptive statistics table |
| `09_external_data.R` | Downloads/caches the six FRED series described above. | `data-clean/external_daily.csv` | feeds `R/10` onward |
| `10_build_panel_extended.R` | Builds the extended panel: adds an RMB-numeraire outcome, a **forward-rolled** version of the shock dates (weekend/holiday announcements mapped to the next trading day instead of dropped), and the merged control series. | `data-clean/reg_data_ext_main.csv` (the panel every later script uses), `output/tables/roll_reconciliation.csv`, `numeraire_coverage.csv` | attrition/data-construction detail |
| `11_numeraire.R` | Robustness of the main result to the rolled shock series; decomposes the RMB-numeraire result into `beta_USD - beta_CNY`. | `output/tables/numeraire_roll_robustness.csv`, `numeraire_decomposition.csv` | numeraire robustness section |
| `12_sample_split.R` | The regime-split analysis (H3): tests whether the FX response differs before/after 11 Aug 2015, via a `shock x post_split` interaction. | `output/tables/split_event_counts.csv`, `split_by_period.csv`, `split_singapore.csv` | H3 regime table |
| `13_regime_stress_tests.R` | Six robustness checks on the H3 regime break (windows/measures, controls, leave-one-out, alternative split dates, mechanism decomposition, economic magnitude). | `output/tables/regime_windows.csv`, `regime_leave_one_out.csv`, `regime_alt_splits.csv`, `regime_mechanism.csv` | H3 robustness appendix |
| `14_dollar_control_fix.R` | Re-runs the H3 control ladder with a dollar index that excludes the RMB and ASEAN-5 currencies, contrasted against the broad index that does not. | `output/tables/regime_dollar_control.csv` | H3 robustness appendix |
| `15_mediator_or_confounder.R` | The mediation/decomposition analysis: does the post-2015 effect run through a dollar-factor channel? Point estimates only (no bootstrap CI -- see `R/19`). | `output/tables/mediation_path_a.csv`, `mediation_decomposition.csv`, `mediation_timing.csv` | mediation section |
| `16_instrument_mix.R` | Documents the change in PBoC policy-instrument mix (quantity tools pre-2015 vs. price tools after) and tests whether the FX response differs by instrument type. | `output/tables/instrument_composition.csv`, `instrument_flags.csv`, `instrument_span.csv` | instrument-mix section |
| `17_instrument_tests.R` | Formal tests on the price-vs-quantity claim in `R/16`, including a leave-one-out check and a dollar-control check. | `output/tables/instrument_tests_claim1.csv`, `_claim2.csv`, `_loo.csv` | instrument-mix section |
| `18_fomc_regime_robustness.R` | Re-estimates the H3 regime interaction excluding event dates within 1 trading day of a scheduled FOMC decision (headline cut), excluding all of 2020, and excluding dates near *any* FOMC meeting including unscheduled ones (sensitivity cut), alongside the baseline. | `output/tables/regime_fomc_2020_robustness.csv` | H3 robustness appendix |
| `19_mediation_bootstrap.R` | Cluster bootstrap (resampling event dates, 2,000 replications) giving a 95% CI on the mediation path-*b* coefficient and the indirect effect a×b from `R/15`. | `output/tables/mediation_bootstrap.csv`, `mediation_bootstrap_table.tex` | mediation section |
| `20_heterogeneity_ftest.R` | Joint F-test that Table 3's four country-interaction terms (relative to Indonesia) are all zero, for both shock measures. | `output/tables/heterogeneity_joint_ftest.csv` | H2 robustness |

`R/11` through `R/17` were originally written as standalone supervisor-response
scripts (see `docs/RUN_ME_FIRST.md` for the original hand-off notes) and, for
most of this branch's history, were never called by `run_all.R` -- a fresh
`source("run_all.R")` run did not reproduce the numeraire decomposition, the
H3 regime stress tests, the mediation point estimates, or the instrument-mix
tests, even though all of that is committed to `output/tables/` and appears
in the paper. That has been fixed: `run_all.R` now calls all of R/01-20 in
dependency order (see the comment above the R/09 line in `run_all.R` for how
that order was derived from what each script actually reads and writes, not
its number). A fresh clean-state run of the full 01-20 pipeline was verified
to exit 0 with every table reproduced to floating-point precision.

## Known caveats

**Exact-date matching vs. the forward roll.** The main estimation panel
(`R/03`, used by Tables 1-3 and everything through `R/08`) merges shocks onto
FX dates by exact calendar date. An announcement that falls on a weekend or a
market holiday is simply dropped -- 31 of 102 main-sample announcement dates
never enter the baseline sample this way. `R/10` builds an alternative,
forward-rolled shock series (`shock_1y_roll`) that maps a non-trading-day
announcement to the next trading day instead, and `R/11`'s roll-robustness
check re-estimates the main result on it. The two are not reconciled: the
thesis text should say plainly which one is the reported baseline and which
is the robustness check (see the open decision on this in
`docs/RUN_ME_FIRST.md`, which as of this writing is still unresolved).

**The scheduled-only FOMC exclusion rule.** `R/18`'s headline robustness cut
excludes PBoC announcement dates that fall within 1 trading day of a
*scheduled* FOMC decision only, not unscheduled ones. Scheduled FOMC dates
are fixed on a public calendar well in advance, so their timing is exogenous
to Chinese monetary policy -- that is what makes dropping them a clean test
of FOMC contamination. Unscheduled FOMC meetings (three in 2008, two in
March 2020) are called precisely when global financial conditions are
already disorderly, so excluding those dates too (the "any-FOMC-adjacent"
column, also reported) would selectively strip out the periods where a
global-shock confound is most plausible and bias the check in favor of
finding a clean result. Both cuts are reported; only the scheduled-only one
is the headline.

**Overlapping `[0,+1]` windows.** A few main-sample announcement dates are
one trading day apart, so one event's `+1` day is the next event's `0` day
(e.g. 2012-07-03 and 2012-07-05). The `fx_return_01` outcome for the earlier
date partly reflects the later shock. This is a standard event-study
trade-off and is not corrected automatically.

**Country-specific FX holidays.** The panel keeps `NA` (not a dropped row) on
a day one ASEAN-5 market was closed but others traded, so per-country sample
sizes differ slightly; `lm()`/`feols()` handle this by list-wise deletion.

**Significance stars.** Every table in this repository -- the modelsummary
tables in `R/05`/`R/06` and the console comparison tables in `R/11`-`R/18` --
now uses the same convention, `* p<0.1, ** p<0.05, *** p<0.01`, set once in
`R/00_setup.R`. This was not always true; if you have console output saved
from before this was fixed, re-run rather than trust the stars in it.

**Reproducing exact digits.** Re-running the pipeline on a different machine
can shift the last one or two digits of coefficients that depend on a chain
of log-return calculations (a normal floating-point artifact of different
BLAS/compiler builds, not a bug). It has not changed any digit that is
actually reported at the precision these tables use. If a fresh run produces
a difference bigger than that, treat it as a real discrepancy and investigate
before trusting the new numbers.

**The FRED cache.** `R/09` and `R/14` both download and cache FRED series
into `data-raw/external/`; those six files are committed, so a fresh clone
does not need internet access to reproduce the pipeline as it stands. Now
that `run_all.R` calls `R/14` before `R/15` and `R/19` (which both need the
advanced-foreign-economies dollar index, `DTWEXAFEGS`, that `R/14` fetches),
a full `run_all.R` run will re-fetch that file itself if it's ever missing.
Running `R/15` or `R/19` on their own without ever having run `R/14` in that
session or having the file cached is the only way this still bites -- they
fail with a clear "run R/14 first" error rather than silently proceeding.

## Status

See `git log` and the individual script headers for the most current account
of what is finished versus in progress; this file summarizes structure and
reproducibility, not day-to-day status.

## Author

Haley J. Fennyery — Erasmus Mundus Joint Master's Degree, Economics of
Globalization and European Integration.

## Supervisor

Dr. Peter Claeys.
