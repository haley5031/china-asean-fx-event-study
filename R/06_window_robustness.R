# =====================================================================
# R/06_window_robustness.R
# Adds [0] (same-day) and [-1,+1] event windows alongside the existing
# [0,+1] baseline, and re-runs the full specification ladder for each
# window and for both the 1Y and 5Y shock measures.
#
# Matched to this repo's conventions:
#   panel        : data-clean/reg_data01_main.csv
#   same-day ret : fx_return       (100 x log-return) == the [0] window
#   [0,+1] col   : fx_return_01     (= fx_return + lead(fx_return), by country)
#   shocks       : shock_1y, shock_5y
#   countries    : idr, myr, php, sgd, thb   (ref = idr)
#   OLS layers   : lm();  FE layers: feols(... cluster = ~date)
#   tables       : modelsummary -> tinytable -> save_tt() into output/tables/
# =====================================================================

source("R/00_setup.R")  # provides paths$out_tables, REF_COUNTRY, ASEAN5

library(dplyr)
library(readr)
library(fixest)
library(modelsummary)
library(tinytable)

# ---- Load cleaned panel --------------------------------------------
panel <- read_csv(file.path("data-clean", "reg_data01_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

# ---- Build the three window returns (within country, date order) ----
# [0]      : same-day  = fx_return
# [0,+1]   : r_t + r_{t+1}
# [-1,+1]  : r_{t-1} + r_t + r_{t+1}
panel <- panel %>%
  group_by(country) %>%
  mutate(
    fx_w0   = fx_return,
    fx_w01  = fx_return + lead(fx_return, 1),
    fx_wm11 = lag(fx_return, 1) + fx_return + lead(fx_return, 1)
  ) %>%
  ungroup()

# ---- Sanity check: reconstructed [0,+1] must match existing column --
chk <- panel %>%
  filter(!is.na(fx_w01), !is.na(fx_return_01)) %>%
  summarise(max_abs_diff = max(abs(fx_w01 - fx_return_01))) %>%
  pull(max_abs_diff)

message(sprintf("Sanity check: max|fx_w01 - fx_return_01| = %.2e", chk))
if (chk > 1e-8) {
  stop("Reconstructed [0,+1] does not match fx_return_01. Stopping -- check column names / ordering.")
}

# ---- Helper: run the full ladder for one window column + one shock --
run_ladder <- function(dt, ycol, shockcol) {
  f_country <- reformulate(shockcol, response = ycol)
  f_pool    <- reformulate(c(shockcol, "factor(country)"), response = ycol)
  f_fe      <- as.formula(sprintf("%s ~ %s | country", ycol, shockcol))
  f_het     <- as.formula(sprintf("%s ~ %s * i(country, ref = '%s') | country",
                                   ycol, shockcol, REF_COUNTRY))

  countries <- sort(unique(dt$country))
  country_models <- lapply(countries, function(cc) {
    lm(f_country, data = dplyr::filter(dt, country == cc))
  })
  names(country_models) <- countries

  list(
    country = country_models,
    pooled  = lm(f_pool, data = dt),
    fe      = feols(f_fe,  data = dt, cluster = ~date),
    het     = feols(f_het, data = dt, cluster = ~date)
  )
}

# ---- Run for every window x shock measure --------------------------
windows <- c(w0 = "fx_w0", w01 = "fx_w01", wm11 = "fx_wm11")
shocks  <- c("shock_1y", "shock_5y")

results <- list()
for (w in names(windows)) {
  for (s in shocks) {
    results[[paste(w, s, sep = "_")]] <- run_ladder(panel, windows[[w]], s)
  }
}

# ---- Console summary 1: FE shock coef / SE / p across all 6 cells ----
fe_summ <- bind_rows(lapply(names(results), function(key) {
  m  <- results[[key]]$fe
  ct <- as.data.frame(coeftable(m))
  data.frame(
    spec = key,
    coef = ct[1, "Estimate"],
    se   = ct[1, "Std. Error"],
    pval = ct[1, "Pr(>|t|)"],
    nobs = m$nobs,
    row.names = NULL
  )
}))
cat("\n=== FE shock coefficient across windows x shocks ===\n")
print(fe_summ, digits = 3)

# ---- Console summary 2: Singapore vs Indonesia across windows --------
het_summ <- bind_rows(lapply(names(results), function(key) {
  m  <- results[[key]]$het
  ct <- as.data.frame(coeftable(m))
  ct$term <- rownames(ct)
  sg <- ct[grepl("sgd", ct$term, ignore.case = TRUE), ]
  if (nrow(sg) == 0) return(NULL)
  data.frame(
    spec = key,
    term = sg$term,
    coef = sg$Estimate,
    se   = sg$`Std. Error`,
    pval = sg$`Pr(>|t|)`,
    row.names = NULL
  )
}))
cat("\n=== Singapore-vs-Indonesia interaction across windows ===\n")
print(het_summ, digits = 3)

# ---- Export LaTeX/HTML tables (modelsummary + tinytable) ------------
gof_keep <- c("nobs", "r.squared")

export_fe <- function(shock_key, label) {
  mods <- list(
    "[0]"      = results[[paste0("w0_",   shock_key)]]$fe,
    "[0,+1]"   = results[[paste0("w01_",  shock_key)]]$fe,
    "[-1,+1]"  = results[[paste0("wm11_", shock_key)]]$fe
  )
  tt <- modelsummary(
    mods,
    output    = "tinytable",
    stars     = c("*" = .1, "**" = .05, "***" = .01),
    gof_map   = gof_keep,
    coef_rename = c("shock_1y" = "Chinese MP shock (1Y)",
                    "shock_5y" = "Chinese MP shock (5Y)"),
    title     = sprintf("Window robustness: FE estimates (%s)", label)
  )
  save_tt(tt, file.path(paths$out_tables,
          sprintf("window_robustness_fe_%s.tex", shock_key)), overwrite = TRUE)
  save_tt(tt, file.path(paths$out_tables,
          sprintf("window_robustness_fe_%s.html", shock_key)), overwrite = TRUE)
}

export_fe("shock_1y", "1Y")
export_fe("shock_5y", "5Y")

cat("\nDone. Tables written to", paths$out_tables, "\n")
