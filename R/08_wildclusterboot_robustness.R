# =============================================================================
# 08_wildclusterboot_robustness.R
# Robustness DIAGNOSTIC ONLY: wild cluster bootstrap-t p-values (Rademacher
# weights, clustered by COUNTRY, G = 5) reported side-by-side with the
# existing date-clustered analytic SEs already in panel_results_table and
# heterogeneity_results_table. This script does not modify, overwrite, or
# re-estimate any existing model or table -- it reads the already-fitted
# objects from output/models.rds and only adds a new, separate table.
#
# Preferred baseline / headline specification and its date-clustered SEs
# (R/04_estimate.R, R/05_tables_figures.R) are UNCHANGED and remain primary.
#
# NOTE ON CLUSTER DIMENSION:
#   fe_1y / fe_5y / fe_het_1y / fe_het_5y all cluster analytic SEs by DATE
#   (~3,101 unique dates in reg_data01_main.csv). The wild cluster bootstrap
#   run here instead clusters by COUNTRY (G = 5, the ASEAN-5), since that is
#   the few-cluster dimension of concern. This means the two p-value columns
#   in the output table answer different questions (robustness to
#   within-date cross-country correlation vs. robustness to within-country
#   serial correlation) -- they are not two estimates of the same SE.
#
# Install once if needed:
# install.packages(c("fwildclusterboot", "modelsummary", "tinytable"))
#
# Input : output/models.rds   (fe_1y, fe_5y, fe_het_1y, fe_het_5y)
# Output: output/tables/wildboot_comparison_table.{html,tex}
#         output/wildboot_results.rds  (raw boottest() output, for review)
# =============================================================================

source("R/00_setup.R")
library(fwildclusterboot)
library(modelsummary)
library(tinytable)
library(dplyr)

models <- readRDS(file.path(paths$output_root, "models.rds"))

# --- Bootstrap settings -------------------------------------------------------
B_REPS     <- 999          # requested replications (see G=5 note below)
BOOT_TYPE  <- "rademacher" # default per instructions
IMPOSE_NULL <- TRUE        # WCR (null imposed) -- standard default (Cameron & Miller 2015)
BOOT_SEED  <- 20260710     # fixed seed for reproducibility across reruns

# NOTE (flag, not changed): with only G = 5 clusters, Rademacher weights admit
# at most 2^5 = 32 distinct sign patterns. fwildclusterboot detects this and
# automatically switches from B random draws to full enumeration of all
# combinations whenever the requested B exceeds the number of possible
# patterns -- i.e. it will very likely enumerate all 32 combinations rather
# than draw 999 random ones. This is documented, intended behavior in
# fwildclusterboot for small G (an exact/deterministic bootstrap), not an
# error. Some authors (Webb 2014) recommend the 6-point "Webb" weight
# distribution instead of Rademacher specifically because G = 5 is at the low
# end of what Rademacher's +-1 enumeration resolves well. We keep Rademacher
# as instructed; swap type = "rademacher" for type = "webb" below if you want
# to check sensitivity to that choice.

# --- Helper: run boottest() for one (model, coefficient) pair ----------------
run_boottest <- function(model, param, model_label) {
  res <- tryCatch(
    boottest(
      object      = model,
      param       = param,
      clustid     = "country",
      B           = B_REPS,
      type        = BOOT_TYPE,
      impose_null = IMPOSE_NULL,
      seed        = BOOT_SEED
    ),
    error = function(e) {
      warning(sprintf("boottest() failed for model = %s, param = %s: %s",
                       model_label, param, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(res)) {
    return(list(
      raw = NULL,
      row = data.frame(model = model_label, term = param,
                        boot_p = NA_real_, boot_tstat = NA_real_,
                        boot_ci_low = NA_real_, boot_ci_high = NA_real_)
    ))
  }
  list(
    raw = res,
    row = data.frame(
      model       = model_label,
      term        = param,
      boot_p      = res$p_val,
      boot_tstat  = unname(res$t_stat),
      boot_ci_low = res$conf_int[1],
      boot_ci_high= res$conf_int[2]
    )
  )
}

# --- Coefficients of interest, matching column names already used in
#     05_tables_figures.R's het_map so terms line up exactly -----------------
het_params_1y <- c("shock_1y",
                    "shock_1y:country::myr", "shock_1y:country::php",
                    "shock_1y:country::sgd", "shock_1y:country::thb")
het_params_5y <- c("shock_5y",
                    "shock_5y:country::myr", "shock_5y:country::php",
                    "shock_5y:country::sgd", "shock_5y:country::thb")

# --- Run wild cluster bootstrap for each requested specification ------------
boot_out <- list()
boot_out[["FE (1Y)"]] <- list(run_boottest(models$fe_1y, "shock_1y", "FE (1Y)"))
boot_out[["FE (5Y)"]] <- list(run_boottest(models$fe_5y, "shock_5y", "FE (5Y)"))
boot_out[["FE Heterogeneity (1Y)"]] <- lapply(
  het_params_1y, function(p) run_boottest(models$fe_het_1y, p, "FE Heterogeneity (1Y)")
)
boot_out[["FE Heterogeneity (5Y)"]] <- lapply(
  het_params_5y, function(p) run_boottest(models$fe_het_5y, p, "FE Heterogeneity (5Y)")
)

boot_rows <- bind_rows(lapply(unlist(boot_out, recursive = FALSE), function(x) x$row))

# Save raw boottest() objects too, in case you want conf_int, t_stat detail,
# or diagnostics (e.g. number of enumerated vs. drawn combinations) later.
boot_raw <- lapply(unlist(boot_out, recursive = FALSE), function(x) x$raw)
saveRDS(list(rows = boot_rows, raw = boot_raw),
        file.path(paths$output_root, "wildboot_results.rds"))

# --- Pull the existing (unchanged) date-clustered analytic SEs for the
#     SAME coefficients, purely for side-by-side display -- the source
#     models are read-only here, nothing is re-estimated. --------------------
extract_clustered <- function(model, terms, model_label) {
  ct <- as.data.frame(coeftable(model))
  ct$term <- rownames(ct)
  ct <- ct[ct$term %in% terms, ]
  data.frame(
    model      = model_label,
    term       = ct$term,
    estimate   = ct$Estimate,
    cluster_se = ct$`Std. Error`,
    cluster_p  = ct$`Pr(>|t|)`,
    row.names  = NULL
  )
}

clustered_rows <- bind_rows(
  extract_clustered(models$fe_1y,     "shock_1y", "FE (1Y)"),
  extract_clustered(models$fe_5y,     "shock_5y", "FE (5Y)"),
  extract_clustered(models$fe_het_1y, het_params_1y, "FE Heterogeneity (1Y)"),
  extract_clustered(models$fe_het_5y, het_params_5y, "FE Heterogeneity (5Y)")
)

comparison <- left_join(clustered_rows, boot_rows, by = c("model", "term"))

# --- Term labels, matching 05_tables_figures.R's het_map naming -------------
term_labels <- c(
  "shock_1y" = "Chinese MP shock (1Y)",
  "shock_5y" = "Chinese MP shock (5Y)",
  "shock_1y:country::myr" = "Malaysia rel. to Indonesia (1Y)",
  "shock_1y:country::php" = "Philippines rel. to Indonesia (1Y)",
  "shock_1y:country::sgd" = "Singapore rel. to Indonesia (1Y)",
  "shock_1y:country::thb" = "Thailand rel. to Indonesia (1Y)",
  "shock_5y:country::myr" = "Malaysia rel. to Indonesia (5Y)",
  "shock_5y:country::php" = "Philippines rel. to Indonesia (5Y)",
  "shock_5y:country::sgd" = "Singapore rel. to Indonesia (5Y)",
  "shock_5y:country::thb" = "Thailand rel. to Indonesia (5Y)"
)

stars_of <- function(p) {
  ifelse(is.na(p), "",
  ifelse(p < .01, "***",
  ifelse(p < .05, "**",
  ifelse(p < .1,  "*", ""))))
}

comparison_fmt <- comparison %>%
  mutate(
    Term                          = unname(term_labels[term]),
    Estimate                      = sprintf("%.3f", estimate),
    `SE (clustered, date)`        = sprintf("%.3f", cluster_se),
    `p (clustered, date)`         = sprintf("%.3f%s", cluster_p, stars_of(cluster_p)),
    `p (wild boot, country)`      = sprintf("%.3f%s", boot_p, stars_of(boot_p)),
    `95% CI (wild boot, country)` = sprintf("[%.3f, %.3f]", boot_ci_low, boot_ci_high)
  ) %>%
  select(Model = model, Term, Estimate,
         `SE (clustered, date)`, `p (clustered, date)`,
         `p (wild boot, country)`, `95% CI (wild boot, country)`)

# --- Export as a new, separate table (tinytable, matching existing format) --
tt_compare <- tt(
  comparison_fmt,
  caption = paste0(
    "Robustness Diagnostic: Clustered SE vs. Wild Cluster Bootstrap p-values ",
    "(bootstrap clustered by country, G = 5)"
  )
) |>
  format_tt(escape = TRUE) |>
  style_tt(align = "llrrrrr")

save_tt(tt_compare, file.path(paths$out_tables, "wildboot_comparison_table.tex"), overwrite = TRUE)
save_tt(tt_compare, file.path(paths$out_tables, "wildboot_comparison_table.html"), overwrite = TRUE)

message("Wild cluster bootstrap diagnostic written to ",
        file.path(paths$out_tables, "wildboot_comparison_table.tex"),
        " (existing tables untouched).")
