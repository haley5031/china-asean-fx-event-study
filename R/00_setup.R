# =============================================================================
# 00_setup.R
# Packages, project paths, and shared constants.
# Sourced at the top of every other script so paths are defined in ONE place.
#
# REVISION (supervisor response): adds
#   - roll_to_trading_day()  : maps announcement dates onto the FX trading
#                              calendar, so weekend/holiday announcements are
#                              rolled forward rather than silently dropped.
#   - MAX_ROLL_DAYS          : cap on how far an announcement may be rolled.
#   - SPLIT_DATE             : institutional sample-split date.
#   - external data paths / FRED series ids.
# =============================================================================

# --- Packages ----------------------------------------------------------------
# install.packages(c("readr", "readxl", "dplyr", "tidyr", "lubridate",
#                    "fixest", "modelsummary", "tinytable", "ggplot2"))

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)

# --- Project paths -----------------------------------------------------------
paths <- list(
  raw_policy   = "data-raw/policy",
  raw_fx       = "data-raw/fx",
  raw_external = "data-raw/external",
  clean        = "data-clean",
  output_root  = "output",
  out_tables   = "output/tables",
  out_figures  = "output/figures"
)

for (d in c(paths$raw_external, paths$clean, paths$output_root,
            paths$out_tables, paths$out_figures)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Shared constants --------------------------------------------------------
SAMPLE_START <- as.Date("2008-01-01")
SAMPLE_END   <- as.Date("2020-05-29")

ASEAN5      <- c("idr", "myr", "php", "sgd", "thb")
REF_COUNTRY <- "idr"

# Maximum number of calendar days an announcement may be rolled forward to
# reach the next trading day. Four covers a Friday-evening announcement
# followed by a Monday holiday. Anything further is left unmatched rather than
# attached to a distant trading day.
MAX_ROLL_DAYS <- 4L

# Institutional sample split. 2015 is chosen ex ante on three coinciding
# grounds, all documented in the write-up:
#   (i)   the 2014-15 Chinese property downturn and the PBoC easing/
#         intervention cycle it triggered;
#   (ii)  the August 2015 RMB fixing reform, after which CNY/USD ceases to be
#         near-constant (so USD and RMB numeraires can diverge);
#   (iii) Das and Song (2022): interest-rate liberalisation "largely completed"
#         with the removal of the deposit-rate ceiling in 2015.
SPLIT_DATE <- as.Date("2015-08-11")   # RMB fixing reform announcement

# --- FRED series used for the numeraire and control exercises ----------------
# Pulled by R/09_external_data.R via the public fredgraph CSV endpoint
# (no API key required).
FRED_SERIES <- c(
  cny_per_usd   = "DEXCHUS",     # China / U.S. FX rate, CNY per USD
  vix           = "VIXCLS",      # CBOE volatility index
  broad_dollar  = "DTWEXBGS",    # Nominal broad U.S. dollar index
  ust_2y        = "DGS2",        # 2-year Treasury constant maturity yield
  brent         = "DCOILBRENTEU" # Brent crude, USD/barrel
)

# --- Robust date parsers -----------------------------------------------------
# The raw policy file is stored in US M/D/YY format ("8/19/06"). It may also
# appear in ISO format ("2006-08-19") if re-exported. We decide per value by
# looking for a slash, because lubridate::ymd() does NOT cleanly fail on
# "8/19/06" -- it silently returns a wrong date.

parse_date_robust <- function(x) {
  x   <- trimws(as.character(x))
  out <- as.Date(rep(NA, length(x)), origin = "1970-01-01")
  is_slash <- grepl("/", x)
  if (any(is_slash))
    out[is_slash]  <- suppressWarnings(lubridate::mdy(x[is_slash], quiet = TRUE))
  if (any(!is_slash))
    out[!is_slash] <- suppressWarnings(lubridate::ymd(x[!is_slash], quiet = TRUE))
  out
}

parse_datetime_robust <- function(x) {
  x   <- trimws(as.character(x))
  out <- as.POSIXct(rep(NA, length(x)), origin = "1970-01-01", tz = "UTC")
  is_slash <- grepl("/", x)
  if (any(is_slash))
    out[is_slash]  <- suppressWarnings(lubridate::mdy_hm(x[is_slash], quiet = TRUE))
  if (any(!is_slash))
    out[!is_slash] <- suppressWarnings(lubridate::ymd_hm(x[!is_slash], quiet = TRUE))
  out
}

# --- Trading-day roll --------------------------------------------------------
# Maps each announcement date onto the first trading day on or after it.
#
# Rationale. The Das and Song (2022) `date` field is already the day on which
# the swap market reprices the announcement (their `datetime` is frequently the
# preceding evening). When that repricing day is a weekend or an ASEAN market
# holiday there is no FX observation to attach the surprise to. Under exact-date
# matching the announcement is dropped entirely; under a forward roll it is
# attached to the next session, which is the first opportunity ASEAN markets
# have to price it. The forward direction matters: rolling backwards would
# attach a surprise to a session that closed before the announcement existed.
#
# Arguments
#   event_dates    Date vector of announcement dates.
#   trading_days   Date vector of available trading days (the FX calendar).
#   max_roll       Integer cap on calendar days rolled (default MAX_ROLL_DAYS).
#
# Returns a data.frame with one row per input date:
#   event_date     the original announcement date
#   matched_date   the trading day it maps to, or NA if none within max_roll
#   roll_days      calendar days rolled (0 = already a trading day)
roll_to_trading_day <- function(event_dates,
                                trading_days,
                                max_roll = MAX_ROLL_DAYS) {

  event_dates  <- as.Date(event_dates)
  trading_days <- sort(unique(as.Date(trading_days)))

  # findInterval on a sorted calendar: for each event date, locate the last
  # trading day strictly before it, so the next one is index + 1. Exact matches
  # are handled separately and get roll_days = 0.
  idx_next <- findInterval(event_dates, trading_days, left.open = TRUE) + 1L
  idx_next[idx_next > length(trading_days)] <- NA_integer_

  matched <- trading_days[idx_next]
  rolled  <- as.integer(matched - event_dates)

  # Exact hits: event date is itself a trading day.
  exact <- event_dates %in% trading_days
  matched[exact] <- event_dates[exact]
  rolled[exact]  <- 0L

  # Enforce the cap.
  too_far <- !is.na(rolled) & rolled > max_roll
  matched[too_far] <- as.Date(NA)
  rolled[too_far]  <- NA_integer_

  data.frame(
    event_date   = event_dates,
    matched_date = matched,
    roll_days    = rolled,
    stringsAsFactors = FALSE
  )
}

# --- Small helper: tidy a fixest/lm coefficient row --------------------------
# Used by the numeraire and control scripts to build comparison tables without
# depending on broom.
coef_row <- function(model, term, label = term, extra = list()) {
  ct <- as.data.frame(coeftable(model))
  if (!term %in% rownames(ct)) {
    hit <- grep(term, rownames(ct), fixed = TRUE, value = TRUE)
    if (length(hit) == 0) stop("term not found in model: ", term)
    term <- hit[1]
  }
  out <- data.frame(
    term  = label,
    coef  = ct[term, "Estimate"],
    se    = ct[term, "Std. Error"],
    pval  = ct[term, grep("^Pr", names(ct))[1]],
    nobs  = tryCatch(nobs(model), error = function(e) NA_integer_),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
  if (length(extra)) out <- cbind(as.data.frame(extra, stringsAsFactors = FALSE), out)
  out
}
