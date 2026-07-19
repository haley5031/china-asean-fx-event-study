# =============================================================================
# 04_estimate.R
# Estimates every model in the thesis from the cleaned main-sample panel and
# saves the fitted objects so the tables/figures script never re-runs them.
#
# Input : data-clean/reg_data01_main.csv
# Output: output/models.rds   (named list of all fitted model objects)
#
# Preferred baseline: country fixed effects, [0,+1] window, 2008-2020,
# shock_1y, SEs clustered by date.
# =============================================================================

source("R/00_setup.R")

reg <- read_csv(file.path(paths$clean, "reg_data01_main.csv"),
                show_col_types = FALSE)

# --- Country-level OLS: [0,+1], shock_1y -------------------------------------
country_ols <- lapply(ASEAN5, function(cc) {
  lm(fx_return_01 ~ shock_1y, data = filter(reg, country == cc))
})
names(country_ols) <- ASEAN5

# --- Pooled OLS (naive baseline, no country control) --------------------------
pooled_1y <- feols(fx_return_01 ~ shock_1y, data = reg, vcov = ~date)
pooled_5y <- feols(fx_return_01 ~ shock_5y, data = reg, vcov = ~date)

# --- Fixed effects (clustered by date) ---------------------------------------
fe_1y <- feols(fx_return_01 ~ shock_1y | country, cluster = ~date, data = reg)
fe_5y <- feols(fx_return_01 ~ shock_5y | country, cluster = ~date, data = reg)

# --- Fixed-effects heterogeneity (interactions, ref = Indonesia) -------------
fe_het_1y <- feols(
  fx_return_01 ~ shock_1y * i(country, ref = REF_COUNTRY) | country,
  cluster = ~date, data = reg
)
fe_het_5y <- feols(
  fx_return_01 ~ shock_5y * i(country, ref = REF_COUNTRY) | country,
  cluster = ~date, data = reg
)

# --- Save all fitted objects -------------------------------------------------
models <- list(
  country_ols = country_ols,
  pooled_1y   = pooled_1y,
  pooled_5y   = pooled_5y,
  fe_1y       = fe_1y,
  fe_5y       = fe_5y,
  fe_het_1y   = fe_het_1y,
  fe_het_5y   = fe_het_5y
)

saveRDS(models, file.path(paths$output_root, "models.rds"))
message("Estimated and saved ", length(models), " model groups to output/models.rds")
