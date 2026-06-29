# =============================================================================
# 05_tables_figures.R
# Builds the thesis regression tables from the fitted models. Exports HTML +
# LaTeX tables. The country-coefficient and heterogeneity forest plots are
# built separately in R/fig_forest_country.R and R/fig_forest_heterogeneity.R.
#
# Input : output/models.rds
# Output: output/tables/country_results_table.{html,tex}
#         output/tables/panel_results_table.{html,tex}
#         output/tables/heterogeneity_results_table.{html,tex}
# =============================================================================

source("R/00_setup.R")
library(modelsummary)
library(tinytable)
library(ggplot2)

models <- readRDS("output/models.rds")

gof_keep <- c("nobs", "r.squared", "F")
stars    <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)

# --- Table 1: country-level --------------------------------------------------
country_models <- models$country_ols
names(country_models) <- c("Indonesia", "Malaysia", "Philippines",
                           "Singapore", "Thailand")

t1 <- modelsummary(
  country_models,
  coef_map  = c("shock_1y" = "Chinese MP shock (1Y)"),
  gof_map   = gof_keep,
  stars     = stars,
  title     = "Country-Level Results: Chinese Monetary Policy Shocks and ASEAN-5 Exchange Rates",
  notes     = c("Each model is estimated separately by country.",
                "All regressions use the [0,+1] event window over the 2008--2020 main sample.",
                "Dependent variable is daily FX return in local currency per U.S. dollar."),
  output    = "tinytable"
)
save_tt(t1, file.path(paths$out_tables, "country_results_table.html"), overwrite = TRUE)
save_tt(t1, file.path(paths$out_tables, "country_results_table.tex"),  overwrite = TRUE)

# --- Table 2: pooled + FE ----------------------------------------------------
panel_models <- list(
  "Pooled OLS (1Y)" = models$pooled_1y,
  "Pooled OLS (5Y)" = models$pooled_5y,
  "FE (1Y)"         = models$fe_1y,
  "FE (5Y)"         = models$fe_5y
)

t2 <- modelsummary(
  panel_models,
  coef_map = c("shock_1y" = "Chinese MP shock (1Y)",
               "shock_5y" = "Chinese MP shock (5Y)"),
  gof_map  = gof_keep,
  stars    = stars,
  title    = "Pooled and Fixed-Effects Results",
  notes    = c("Pooled OLS models include country indicators.",
               "FE models include country fixed effects and standard errors clustered by date.",
               "All models use the [0,+1] event window over the 2008--2020 main sample."),
  output   = "tinytable"
)
save_tt(t2, file.path(paths$out_tables, "panel_results_table.html"), overwrite = TRUE)
save_tt(t2, file.path(paths$out_tables, "panel_results_table.tex"),  overwrite = TRUE)

# --- Table 3: heterogeneity --------------------------------------------------
het_models <- list(
  "FE Heterogeneity (1Y)" = models$fe_het_1y,
  "FE Heterogeneity (5Y)" = models$fe_het_5y
)

het_map <- c(
  "shock_1y"                       = "Indonesia baseline shock (1Y)",
  "shock_5y"                       = "Indonesia baseline shock (5Y)",
  "shock_1y:country::myr"          = "Malaysia relative to Indonesia",
  "shock_1y:country::php"          = "Philippines relative to Indonesia",
  "shock_1y:country::sgd"          = "Singapore relative to Indonesia",
  "shock_1y:country::thb"          = "Thailand relative to Indonesia",
  "shock_5y:country::myr"          = "Malaysia relative to Indonesia",
  "shock_5y:country::php"          = "Philippines relative to Indonesia",
  "shock_5y:country::sgd"          = "Singapore relative to Indonesia",
  "shock_5y:country::thb"          = "Thailand relative to Indonesia"
)

t3 <- modelsummary(
  het_models,
  coef_map = het_map,
  gof_map  = c("nobs", "r.squared"),
  stars    = stars,
  title    = "Heterogeneity Results: Country Differences Relative to Indonesia",
  notes    = c("Reference category is Indonesia.",
               "All models use the [0,+1] event window and 2008--2020 main sample.",
               "Standard errors are clustered by date."),
  output   = "tinytable"
)
save_tt(t3, file.path(paths$out_tables, "heterogeneity_results_table.html"), overwrite = TRUE)
save_tt(t3, file.path(paths$out_tables, "heterogeneity_results_table.tex"),  overwrite = TRUE)

message("Tables written to output/tables.")
