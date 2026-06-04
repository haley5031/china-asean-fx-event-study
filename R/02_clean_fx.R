# =============================================================================
# 02_clean_fx.R
# Reads the raw ASEAN-5 FX workbook, cleans it to a tidy wide table, and
# computes daily log returns.
#
# Input : data-raw/fx/fx_asean5_filled.xlsx
#         sheet "MAIN_ASEAN_EXCHANGE_RATES" (row 1 is a title; header on row 2)
# Output: data-clean/fx_asean5_clean.csv     (levels, one column per currency)
#         data-clean/fx_returns_wide.csv      (levels + ret_* return columns)
#
# FX convention: local currency units per U.S. dollar. A rise = depreciation.
# Returns: r = 100 * (ln(FX_t) - ln(FX_{t-1})).
# =============================================================================

source("R/00_setup.R")

fx_path <- file.path(paths$raw_fx, "fx_asean5_filled.xlsx")

# --- Read --------------------------------------------------------------------
# skip = 1 drops the title row so the real header ("Date", currency names) is used.
fx_raw <- read_excel(
  fx_path,
  sheet = "MAIN_ASEAN_EXCHANGE_RATES",
  skip  = 1
)

# --- Clean -------------------------------------------------------------------
fx_clean <- fx_raw %>%
  rename(
    date = Date,
    idr  = `Indonesian rupiah (IDR)`,
    myr  = `Malaysian ringgit (MYR)`,
    php  = `Philippine peso (PHP)`,
    sgd  = `Singapore dollar (SGD)`,
    thb  = `Thai baht (THB)`
  ) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

write_csv(fx_clean, file.path(paths$clean, "fx_asean5_clean.csv"))
message("Saved cleaned FX levels: ", nrow(fx_clean), " rows.")

# --- Returns -----------------------------------------------------------------
fx_returns <- fx_clean %>%
  arrange(date) %>%
  mutate(
    ret_idr = 100 * (log(idr) - log(lag(idr))),
    ret_myr = 100 * (log(myr) - log(lag(myr))),
    ret_php = 100 * (log(php) - log(lag(php))),
    ret_sgd = 100 * (log(sgd) - log(lag(sgd))),
    ret_thb = 100 * (log(thb) - log(lag(thb)))
  )

write_csv(fx_returns, file.path(paths$clean, "fx_returns_wide.csv"))
message("Saved wide FX returns: ", nrow(fx_returns), " rows.")
