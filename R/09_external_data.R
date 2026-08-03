# =============================================================================
# 09_external_data.R
# Downloads the external daily series needed for (a) the RMB-numeraire
# decomposition and (b) the control-variable robustness checks.
#
# Source : FRED public CSV endpoint (no API key required).
# Output : data-raw/external/<series>.csv        (raw download, cached)
#          data-clean/external_daily.csv          (tidy wide daily table)
#
# Series
#   DEXCHUS      China / U.S. FX rate, CNY per USD  -> RMB numeraire
#   VIXCLS       CBOE volatility index               -> global risk control
#   DTWEXBGS     Nominal broad U.S. dollar index     -> dollar-leg control
#   DGS2         2-year Treasury yield               -> U.S. monetary signal
#   DCOILBRENTEU Brent crude, USD/barrel             -> commodity control
#
# The download is cached: files already present in data-raw/external are not
# re-fetched, so the pipeline is reproducible offline once run. Delete the
# folder to force a refresh.
# =============================================================================

source("R/00_setup.R")

FRED_START <- "2005-01-01"   # generous lead-in so lagged/differenced series
FRED_END   <- "2020-12-31"   # are defined at the start of the sample

fred_url <- function(id) {
  sprintf(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s&cosd=%s&coed=%s",
    id, FRED_START, FRED_END
  )
}

# --- Fetch (cached) ----------------------------------------------------------
fetch_fred <- function(id) {
  dest <- file.path(paths$raw_external, paste0(id, ".csv"))

  if (!file.exists(dest)) {
    message("Downloading ", id, " from FRED ...")
    ok <- tryCatch({
      download.file(fred_url(id), destfile = dest, quiet = TRUE, mode = "wb")
      TRUE
    }, error = function(e) {
      message("  FAILED: ", conditionMessage(e))
      FALSE
    })
    if (!ok || !file.exists(dest) || file.size(dest) < 50) {
      stop("Could not download ", id, ". Check the connection, or download\n",
           "  ", fred_url(id), "\n",
           "manually and save it as ", dest)
    }
  } else {
    message("Using cached ", basename(dest))
  }

  raw <- read_csv(dest, col_types = cols(.default = col_character()))

  # FRED has used both "DATE" and "observation_date" as the first column name
  # across site revisions; take it positionally so either works.
  if (ncol(raw) < 2) stop("Unexpected file layout for ", id)
  out <- data.frame(
    date  = as.Date(raw[[1]]),
    value = suppressWarnings(as.numeric(raw[[2]])),  # "." missing -> NA
    stringsAsFactors = FALSE
  )
  names(out)[2] <- id
  out[!is.na(out$date), ]
}

series_list <- lapply(FRED_SERIES, fetch_fred)

# --- Merge to one wide daily table -------------------------------------------
external <- Reduce(function(a, b) full_join(a, b, by = "date"), series_list) %>%
  arrange(date)

# Rename FRED ids to the readable names used downstream.
names(external)[match(FRED_SERIES, names(external))] <- names(FRED_SERIES)

# --- Derived transformations -------------------------------------------------
# Log returns for price/index series; first differences (in basis points) for
# the yield. Computed on the raw daily calendar before any merge, so a gap in
# one series never contaminates another.
external <- external %>%
  arrange(date) %>%
  mutate(
    ret_cny          = 100 * (log(cny_per_usd)  - log(lag(cny_per_usd))),
    ret_broad_dollar = 100 * (log(broad_dollar) - log(lag(broad_dollar))),
    ret_brent        = 100 * (log(brent)        - log(lag(brent))),
    d_vix            = vix    - lag(vix),
    d_ust_2y         = 100 * (ust_2y - lag(ust_2y))   # percentage pts -> bps
  )

write_csv(external, file.path(paths$clean, "external_daily.csv"))

# --- Coverage report ---------------------------------------------------------
cover <- external %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END) %>%
  summarise(across(c(cny_per_usd, vix, broad_dollar, ust_2y, brent),
                   ~ sum(!is.na(.x))))

message("\nNon-missing observations, ", SAMPLE_START, " to ", SAMPLE_END, ":")
print(as.data.frame(cover))
message("\nSaved ", nrow(external), " rows to data-clean/external_daily.csv")
