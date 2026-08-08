# =============================================================================
# 19_mediation_bootstrap.R
# R/15 STEP 2 decomposes the post-split total effect into direct + indirect
# (a*b), where path a is shock -> dollar factor and path b is dollar factor
# -> ASEAN FX return, both estimated with country fixed effects on the
# post-2015 subsample. The point estimate of a*b has no analytical standard
# error (it is a product of two estimated coefficients), so R/15 reports it
# without inference.
#
# path_a below replicates R/15 STEP 2's path_a model exactly (panel,
# country FE, clustered by date, post-split subsample) -- NOT STEP 1's
# date-level OLS, which the paper reports separately as "Panel A". The two
# give different numbers (STEP 1 is unweighted across dates; the panel
# version here implicitly weights each date by how many of the five
# ASEAN-5 countries have a valid [0,+1] return that day). See R/15's header
# and STEP 1 comment for the full explanation. This bootstrap's path_a and
# its CI should be compared against STEP 2 / this script, never against
# STEP 1 / Panel A's date-level estimate.
#
# This script supplies that inference with a CLUSTER BOOTSTRAP BY EVENT DATE:
# each replication resamples the distinct dates in the post-split panel WITH
# REPLACEMENT (not rows), keeping every country's observation for each
# resampled date, and refits path a and path b on the resulting panel. This
# respects the clustering the point estimates themselves are inferred under
# (SEs clustered by date in R/15) and does not assume independence across
# the five ASEAN currencies on a given announcement day.
#
# Input : data-clean/reg_data_ext_main.csv
#         data-raw/external/DTWEXAFEGS.csv   (fetched by R/14)
# Output: output/tables/mediation_bootstrap.csv
#         output/tables/mediation_bootstrap_table.tex
# =============================================================================

source("R/00_setup.R")

set.seed(20150811)  # reproducibility; digits are the regime split date, not a tuned choice
B <- 2000            # bootstrap replications (>= 1000 requested)

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

SHOCK <- "shock_1y"

# --- Rebuild the AFE dollar control (same construction as R/14 and R/15) -----
afe_file <- file.path(paths$raw_external, "DTWEXAFEGS.csv")
if (!file.exists(afe_file))
  stop("Run R/14_dollar_control_fix.R first -- it downloads DTWEXAFEGS.")

afe_raw <- read_csv(afe_file, col_types = cols(.default = col_character()))
afe <- data.frame(date = as.Date(afe_raw[[1]]),
                  dollar_afe = suppressWarnings(as.numeric(afe_raw[[2]])))
afe <- afe[!is.na(afe$date), ]

afe_ctrl <- panel %>% distinct(date) %>% arrange(date) %>%
  left_join(afe, by = "date") %>%
  mutate(ret_dollar_afe = 100 * (log(dollar_afe) - log(lag(dollar_afe))),
         ctrl_afe_w01   = ret_dollar_afe + lead(ret_dollar_afe, 1)) %>%
  select(date, ctrl_afe_w01)

panel <- panel %>% left_join(afe_ctrl, by = "date")

# =============================================================================
# Cluster-bootstrap one mediator
# =============================================================================
# d is the post-split panel restricted to rows where both the outcome and the
# mediator are observed -- identical sample restriction to R/15 STEP 2, so the
# point estimates reproduce theirs exactly.
bootstrap_mediator <- function(mv, d, B) {

  m_a      <- feols(as.formula(sprintf("%s ~ %s | country", mv, SHOCK)),
                    cluster = ~date, data = d)
  m_direct <- feols(as.formula(sprintf("usd_w01 ~ %s + %s | country", SHOCK, mv)),
                    cluster = ~date, data = d)

  a_hat <- as.data.frame(coeftable(m_a))[SHOCK, "Estimate"]
  b_hat <- as.data.frame(coeftable(m_direct))[mv, "Estimate"]

  # Split once into per-date chunks; each bootstrap replicate resamples date
  # LABELS with replacement and rebinds the corresponding chunks (all
  # countries observed on that date travel together), which is the cluster
  # bootstrap: resampling whole clusters, not individual rows.
  d_split    <- split(d, d$date)
  dates_pool <- names(d_split)
  nD         <- length(dates_pool)

  boot_a <- numeric(B)
  boot_b <- numeric(B)

  for (r in seq_len(B)) {
    samp <- sample(dates_pool, nD, replace = TRUE)
    boot_panel <- bind_rows(d_split[samp])

    ma_b <- tryCatch(
      feols(as.formula(sprintf("%s ~ %s | country", mv, SHOCK)),
            data = boot_panel, notes = FALSE),
      error = function(e) NULL)
    md_b <- tryCatch(
      feols(as.formula(sprintf("usd_w01 ~ %s + %s | country", SHOCK, mv)),
            data = boot_panel, notes = FALSE),
      error = function(e) NULL)

    if (is.null(ma_b) || is.null(md_b)) { boot_a[r] <- NA; boot_b[r] <- NA; next }

    ct_a <- as.data.frame(coeftable(ma_b))
    ct_d <- as.data.frame(coeftable(md_b))
    boot_a[r] <- if (SHOCK %in% rownames(ct_a)) ct_a[SHOCK, "Estimate"] else NA
    boot_b[r] <- if (mv    %in% rownames(ct_d)) ct_d[mv,    "Estimate"] else NA
  }

  ok <- !is.na(boot_a) & !is.na(boot_b)
  boot_ab <- boot_a[ok] * boot_b[ok]

  list(
    a_hat = a_hat, b_hat = b_hat, ab_hat = a_hat * b_hat,
    n_dates = nD, n_ok = sum(ok), n_failed = B - sum(ok),
    ci_b  = quantile(boot_b[ok],  c(0.025, 0.975)),
    ci_ab = quantile(boot_ab,     c(0.025, 0.975))
  )
}

mediators <- list(c("AFE dollar", "ctrl_afe_w01"), c("Broad dollar", "ctrl_dollar_w01"))

results <- lapply(mediators, function(m) {
  lab <- m[1]; mv <- m[2]
  d <- panel %>% filter(post_split == 1, !is.na(usd_w01), !is.na(.data[[mv]]))
  r <- bootstrap_mediator(mv, d, B)
  data.frame(
    mediator      = lab,
    path_a        = r$a_hat,
    path_b        = r$b_hat,
    path_b_lo     = r$ci_b[1],
    path_b_hi     = r$ci_b[2],
    indirect_ab   = r$ab_hat,
    indirect_lo   = r$ci_ab[1],
    indirect_hi   = r$ci_ab[2],
    n_dates       = r$n_dates,
    n_boot_ok     = r$n_ok,
    row.names = NULL
  )
})
results <- bind_rows(results)

cat(sprintf("\n=== Cluster bootstrap by event date (B = %d replications) ===\n\n", B))
print(results, row.names = FALSE, digits = 4)
write_csv(results, file.path(paths$out_tables, "mediation_bootstrap.csv"))

cat("\n  path_b_lo/hi     = 95%% percentile CI on path b (dollar factor -> ASEAN return)\n",
    "  indirect_lo/hi   = 95%% percentile CI on the indirect effect a*b\n",
    "  n_dates          = distinct post-split trading dates in the resampling pool\n",
    "  n_boot_ok        = replications that were estimable (both models converged)\n",
    sep = "")

# =============================================================================
# LaTeX table (matches the hand-rolled booktabs-style convention used for
# summary tables in R/08_descriptive_stats.R, rather than the
# modelsummary/tinytable convention used for fixest coefficient tables)
# =============================================================================
fmt <- function(x, d = 3) formatC(as.numeric(x), digits = d, format = "f")

rows <- apply(results, 1, function(r)
  sprintf("%s & %s & %s & [%s, %s] & %s & [%s, %s] \\\\",
          r["mediator"],
          fmt(r["path_a"]),
          fmt(r["path_b"]),  fmt(r["path_b_lo"]),   fmt(r["path_b_hi"]),
          fmt(r["indirect_ab"]), fmt(r["indirect_lo"]), fmt(r["indirect_hi"]))
)

tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Mediation Decomposition, Post-2015 Subsample: Cluster-Bootstrap Inference}",
  "\\label{tab:mediationboot}",
  "\\begin{tabular}{lccccc}",
  "\\hline",
  " & Path $a$ & Path $b$ & 95\\% CI ($b$) & Indirect $a \\times b$ & 95\\% CI ($a \\times b$) \\\\",
  "\\hline",
  rows,
  "\\hline",
  "\\end{tabular}",
  paste0(
    "\\caption*{\\footnotesize\\textit{Note:} Path $a$ is the effect of the ",
    "one-year shock on the dollar-factor mediator; path $b$ is the effect of ",
    "the mediator on ASEAN-5 USD returns holding the shock fixed; both are ",
    "estimated with country fixed effects on the post-2015 subsample (", B,
    " bootstrap replications, clustered by event date). Confidence intervals ",
    "are 95\\% percentile intervals from resampling event dates with ",
    "replacement.}"
  ),
  "\\end{table}"
)

out_path <- file.path(paths$out_tables, "mediation_bootstrap_table.tex")
writeLines(tex, out_path)
message("\nSaved: output/tables/mediation_bootstrap.csv")
message("Saved: ", out_path)
message("Upload to Overleaf as Tables/mediation_bootstrap_table.tex")
