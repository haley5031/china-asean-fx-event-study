# =============================================================================
# 20_heterogeneity_ftest.R
# R/04 and Table 3 (heterogeneity_results_table) report the H2 heterogeneity
# specification -- shock_1y interacted with country dummies, Indonesia as the
# reference -- coefficient by coefficient. Only the Singapore interaction is
# individually significant (at 10%), which is a claim about ONE contrast, not
# about whether the four countries differ from Indonesia as a group.
#
# This script runs the joint test the individual coefficients cannot answer:
# an F-test (Wald) that the four non-reference interaction terms
# (shock_1y:country::myr/php/sgd/thb) are jointly zero.
#
# Input : output/models.rds   (fe_het_1y, fe_het_5y, from R/04)
# Output: output/tables/heterogeneity_joint_ftest.csv
# =============================================================================

source("R/00_setup.R")

if (!file.exists(file.path(paths$output_root, "models.rds")))
  stop("Run R/04_estimate.R first -- it saves output/models.rds.")

models <- readRDS(file.path(paths$output_root, "models.rds"))

joint_test <- function(model, shock) {
  w <- wald(model, paste0(shock, ":country"))
  data.frame(
    shock = shock,
    F_stat = w$stat,
    df1    = w$df1,
    df2    = w$df2,
    pval   = w$p,
    vcov   = w$vcov,
    row.names = NULL
  )
}

cat("\n=== H2: joint test that the four country interactions (relative to Indonesia) are zero ===\n\n")

cat("--- shock_1y (main specification, [0,+1], 2008-2020) ---\n")
w1 <- wald(models$fe_het_1y, "shock_1y:country")
print(w1)

cat("\n--- shock_5y (robustness measure) ---\n")
w5 <- wald(models$fe_het_5y, "shock_5y:country")
print(w5)

ftest_tab <- bind_rows(
  joint_test(models$fe_het_1y, "shock_1y"),
  joint_test(models$fe_het_5y, "shock_5y")
)

cat("\n\n=== Summary ===\n\n")
print(ftest_tab, row.names = FALSE, digits = 4)
write_csv(ftest_tab, file.path(paths$out_tables, "heterogeneity_joint_ftest.csv"))

cat("\nH0: shock_1y:country::myr = shock_1y:country::php = shock_1y:country::sgd = shock_1y:country::thb = 0\n")
cat(sprintf(
  "F(%d, %.0f) = %.3f, p = %.4f.\n",
  ftest_tab$df1[1], ftest_tab$df2[1], ftest_tab$F_stat[1], ftest_tab$pval[1]
))
if (ftest_tab$pval[1] >= 0.05) {
  cat("The joint null is NOT rejected at 5%: the individually significant\n",
      "Singapore coefficient does not establish that ASEAN-5 responses differ\n",
      "from Indonesia's as a group. Report the Singapore contrast as\n",
      "suggestive, not as evidence of general cross-country heterogeneity.\n",
      sep = "")
} else {
  cat("The joint null IS rejected at 5%: the four country interactions are not\n",
      "all zero, supporting cross-country heterogeneity beyond the single\n",
      "Singapore contrast.\n", sep = "")
}

message("\nSaved: output/tables/heterogeneity_joint_ftest.csv")
