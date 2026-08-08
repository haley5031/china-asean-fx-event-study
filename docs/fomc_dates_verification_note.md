# FOMC meeting dates, 2008–2020 — verification note

Replaces the file generated from model memory in branch
`claude/thesis-robustness-checks-kfhnym`. Every row here traces to a primary
Federal Reserve source, recorded per-row in the `source` column.

## Sources

| Tag | Source | Years |
|---|---|---|
| `FED_HIST` | federalreserve.gov FOMC historical materials page (lists meetings as actually held) | 2008 |
| `FED_SCHED` | federalreserve.gov FOMC schedule-announcement press releases | 2009–2019 |
| `FRASER` | FRASER (St. Louis Fed) FOMC meeting index (lists meetings as actually held) | 2020 |

103 scheduled meetings + 5 unscheduled meetings that produced a public
announcement. Validation run at build time: no weekend dates (bar the one known
Sunday event), no duplicate decision dates, no meeting spanning more than two
days, exactly eight scheduled meetings in every year except 2020.

## Two design changes to make in `R/18_fomc_regime_robustness.R`

**1. Flag on `decision_date`, not on the meeting start.** The date that matters
for an FX event study is the day the statement reached markets, which is the
second day of a two-day meeting. Three of the 2008 entries and the 2 March 2020
entry have a `decision_date` that differs from `meeting_end` because the
statement followed a conference call by one day; those are recorded separately.

**2. Restrict the exclusion to `meeting_type == "scheduled"`.** Scheduled FOMC
dates are pre-announced, so their timing is exogenous to anything happening in
Chinese monetary policy — which is the whole point of the check, and matches
the wording already in the draft ("within one trading day of a scheduled FOMC
meeting"). Unscheduled meetings are endogenous to crisis conditions, so
excluding them would remove exactly the dates where a global-shock confound is
most likely and would bias the check in the paper's favour. They are included
in the file with `meeting_type == "unscheduled"` so the sensitivity can be
reported, but they should not drive the headline exclusion.

Conservative construction: where a meeting's exact span was ambiguous, the
wider two-day span is recorded. This flags more event dates rather than fewer,
so the robustness check is harder to pass, not easier.

## Five rows worth a spot-check

These are the only rows where the schedule announcement listed a one-day
meeting that was later expanded to two days. The expansions are well
documented, but they are the rows where a transcription error would be
invisible, and they are the ones to confirm against the Fed's historical
materials page for that year before this goes in the draft.

| Meeting | Recorded span | Recorded decision date |
|---|---|---|
| Mar 2009 | 17–18 Mar | 18 Mar 2009 |
| Aug 2009 | 11–12 Aug | 12 Aug 2009 |
| Sep 2009 | 22–23 Sep | 23 Sep 2009 |
| Dec 2009 | 15–16 Dec | 16 Dec 2009 |
| Sep 2011 | 20–21 Sep | 21 Sep 2011 |

If any is wrong, the error moves a decision date by one day, which can only
add or drop an event date at the margin of the ±1 trading day window. Re-run
`R/18` after any correction.

## One thing that is not an error

2020 has seven scheduled meetings, not eight. The 17–18 March 2020 meeting was
cancelled and superseded by the unscheduled meeting of 15 March. If a
validation script asserts eight scheduled meetings per year, it will fail on
2020; the assertion is wrong, not the data.
