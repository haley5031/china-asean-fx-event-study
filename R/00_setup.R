# =============================================================================
# 00_setup.R
# Packages, project paths, and shared constants.
# Sourced at the top of every other script so paths are defined in ONE place.
# =============================================================================

# --- Packages ----------------------------------------------------------------
# Install once if needed:
# install.packages(c("readr", "readxl", "dplyr", "tidyr", "lubridate",
#                    "fixest", "modelsummary", "tinytable", "ggplot2"))

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(fixest)

# --- Project paths -----------------------------------------------------------
# All paths are relative to the project root (the folder holding the .Rproj
# file). Open the project via the .Rproj file so the working directory is set
# correctly and these resolve. Do not use setwd().

paths <- list(
  raw_policy   = "data-raw/policy",
  raw_fx       = "data-raw/fx",
  clean        = "data-clean",
  output_root  = "output",
  out_tables   = "output/tables",
  out_figures  = "output/figures"
)

# Create output directories if missing. git does not track empty folders, so a
# fresh clone may not contain output/tables and output/figures yet. Creating
# them here means every script can write its outputs without a prior step
# having to exist first.
for (d in c(paths$clean, paths$output_root, paths$out_tables, paths$out_figures)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Shared constants --------------------------------------------------------
# Main estimation sample. Defined once so every script and the write-up agree.
SAMPLE_START <- as.Date("2008-01-01")
SAMPLE_END   <- as.Date("2020-05-29")

# ASEAN-5 currency codes used as the country identifier throughout the panel.
ASEAN5 <- c("idr", "myr", "php", "sgd", "thb")

# Reference country for heterogeneity interactions.
REF_COUNTRY <- "idr"

# --- Robust date parsers -----------------------------------------------------
# The raw policy file is stored in US M/D/YY format ("8/19/06"). It may also
# appear in ISO format ("2006-08-19") if re-exported. We decide per value by
# looking for a slash, because lubridate::ymd() does NOT cleanly fail on
# "8/19/06" -- it silently returns a wrong date -- so a try-ymd-then-mdy
# fallback misparses slash dates. Routing by format up front avoids that.
# Two-digit years map to 2000-2068 (lubridate default), correct for 2006-2020.

parse_date_robust <- function(x) {
  x   <- trimws(as.character(x))
  out <- as.Date(rep(NA, length(x)), origin = "1970-01-01")
  is_slash <- grepl("/", x)
  if (any(is_slash))
    out[is_slash]  <- suppressWarnings(lubridate::mdy(x[is_slash], quiet = TRUE))  # 8/19/06
  if (any(!is_slash))
    out[!is_slash] <- suppressWarnings(lubridate::ymd(x[!is_slash], quiet = TRUE)) # 2006-08-19
  out
}

parse_datetime_robust <- function(x) {
  x   <- trimws(as.character(x))
  out <- as.POSIXct(rep(NA, length(x)), origin = "1970-01-01", tz = "UTC")
  is_slash <- grepl("/", x)
  if (any(is_slash))
    out[is_slash]  <- suppressWarnings(lubridate::mdy_hm(x[is_slash], quiet = TRUE))  # 8/18/06 17:18
  if (any(!is_slash))
    out[!is_slash] <- suppressWarnings(lubridate::ymd_hm(x[!is_slash], quiet = TRUE)) # 2006-08-18 17:18
  out
}
