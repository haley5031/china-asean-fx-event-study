# Supervisor-response revisions — run order

Drop these into your repo, then run in order. Nothing overwrites your existing
outputs: `R/03`–`R/08` and `output/models.rds` are untouched, and the new panel
is a superset written to new filenames.

## Install

| File | Destination | Action |
|---|---|---|
| `00_setup.R` | `R/00_setup.R` | **replaces** existing (pure addition — same constants, plus the roll helper and new paths) |
| `09_external_data.R` | `R/09_external_data.R` | new |
| `10_build_panel_extended.R` | `R/10_build_panel_extended.R` | new |
| `11_numeraire.R` | `R/11_numeraire.R` | new |
| `12_sample_split.R` | `R/12_sample_split.R` | new |

Keep a copy of the old `00_setup.R` until you've confirmed `run_all.R` still
reproduces your current numbers.

## Run

```r
source("run_all.R")              # confirm the existing pipeline still works
source("R/09_external_data.R")   # downloads 5 FRED series (needs internet, no API key)
source("R/10_build_panel_extended.R")
source("R/11_numeraire.R")
source("R/12_sample_split.R")
```

`09` caches downloads in `data-raw/external/`. If FRED is unreachable, the error
message prints the exact URL to fetch by hand.

## Three checks that must pass before any output is trustworthy

`10` prints these. If any fails, stop and send me the console output.

- **(a)** `max|usd_w01 - baseline fx_return_01|` ≈ 0 — the extended panel
  reproduces your current baseline outcome exactly. If not, the two panels have
  diverged and nothing is comparable.
- **(b)** `max|r_cross - (r_usd - r_cny)|` ≈ 0 — the numeraire identity holds.
  The whole decomposition in `11` rests on this arithmetic.
- **(c)** The identity check inside `11` section B: `beta_cross` should equal
  `beta_USD - beta_CNY` to ~1e-12.

## What to send back

1. The **event-date reconciliation table** from `10` — how many of the 31
   currently-dropped announcements the roll recovers, and how far they roll.
2. The **numeraire coverage table** from `10` — how many observations the RMB
   cross rate loses to US holidays.
3. The full **console output of `11`** — sections A through D.
4. The **console output of `12`**.

Then we write. The empirical work is the small part.

---

## Open decisions (don't resolve these alone — flag them when you report back)

**1. Does the roll become the baseline, or stay a robustness check?**
Your draft's Data section currently *claims* rolling is already done. It is not
— `R/03` uses an exact-date `left_join`, which is why 31 announcements vanish
between 102 and 71. Two honest ways out:

- *Promote the roll to baseline.* Matches what the text already says and what
  your supervisor asked for; costs a re-run of every table.
- *Keep exact matching, correct the text,* and report the roll as a robustness
  check showing the null is unaffected. Less rewriting, equally defensible.

Decide after seeing whether the coefficients move. Either way the current
sentence citing Shieh (2024, p. 5) has to change.

**2. `SHOCK <- "shock_1y"` at the top of `11` and `12`.** Flip to
`"shock_1y_roll"` if you promote the roll. Change it in one place, re-run.

**3. `SPLIT_DATE` in `00_setup.R`.** Currently 2015-08-11 (the RMB fixing
reform). If you'd rather anchor on the property downturn itself, 2014-11-22
(the first LDR cut of the easing cycle) is the alternative. Check the event
counts either side before committing — see the top of `12`'s output.

**4. FOMC overlap.** `11` section D controls for the US signal with the 2-year
Treasury change rather than dropping FOMC-overlap dates. This is deliberate: it
needs no FOMC calendar and captures US monetary news on non-FOMC days too. If
you also want the date-drop version, download the announcement dates from
federalreserve.gov (Monetary Policy → FOMC → Historical calendars) rather than
typing them from memory, and I'll write the flag.
