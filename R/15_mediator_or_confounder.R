# =============================================================================
# 15_mediator_or_confounder.R
# The post-2015 interaction attenuates when the dollar factor is controlled for
# (R/13 check 2, R/14). That attenuation has two possible readings, and they
# imply opposite write-ups:
#
#   CONFOUNDER. The dollar moves for reasons unrelated to PBoC announcements,
#     and ASEAN currencies track it. The apparent China effect is spurious
#     co-movement. -> The CONTROLLED estimate is the right one, and the
#     post-2015 result should be reported as largely explained away.
#
#   MEDIATOR. The PBoC surprise itself moves the dollar factor post-2015, and
#     ASEAN currencies respond through that channel. Controlling for the dollar
#     then conditions on a post-treatment variable, removing part of the effect
#     being estimated. -> The UNCONTROLLED estimate is the right one, and the
#     dollar is how the effect travels rather than a rival explanation.
#
# The two are distinguished by one testable implication: under mediation the
# shock must predict the dollar factor; under confounding it must not.
#
# This script runs that test, then decomposes the total effect into direct and
# indirect components, verifying the decomposition identity numerically.
#
# STEP 1 vs STEP 2 -- two different estimators of path a, do not conflate them.
#   STEP 1 ("Panel A" in the paper) reports path a at TWO levels: date-level
#     OLS (one row per calendar date, no country dimension) and, alongside it,
#     the panel estimator with country fixed effects and SEs clustered by
#     date. The panel version there is the SAME specification as path_a in
#     STEP 2 below and in R/19's bootstrap -- it is the preferred estimate,
#     since it matches the convention used everywhere else in the thesis. The
#     date-level and panel numbers differ (documented at STEP 1) because the
#     panel version implicitly weights each date by how many of the five
#     ASEAN-5 countries have a valid [0,+1] return that day, while the
#     date-level version weights every trading day equally.
#   STEP 2's path_a is the panel estimator only, estimated on the post-split
#     subsample used for the decomposition (a slightly different sample than
#     STEP 1's panel estimate, which also covers pre-split). R/19's cluster
#     bootstrap replicates STEP 2's path_a and path_b models exactly.
#
# Input : data-clean/reg_data_ext_main.csv
#         data-raw/external/DTWEXAFEGS.csv   (fetched by R/14)
# Output: output/tables/mediation_*.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

SHOCK <- "shock_1y"

# --- Rebuild the AFE dollar control (same construction as R/14) --------------
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
# STEP 1. Does the shock predict the dollar factor?  (path a)
# =============================================================================
# Reported at TWO levels, side by side, labelled, so they cannot be confused
# (see the header note above):
#   "date-level OLS"               -- one row per calendar date, no country
#                                      dimension, unweighted across dates.
#   "panel, country FE, clustered" -- one row per country-date, country fixed
#                                      effects, SEs clustered by date; the
#                                      same specification used everywhere else
#                                      in the thesis. PREFERRED for the paper.
# The two differ because the panel version implicitly weights each date by
# how many of the five ASEAN-5 countries have a valid [0,+1] return that day
# (coverage is unbalanced -- idr in particular has notably fewer observed
# dates than the other four), while the date-level version weights every
# trading day equally.
dates <- panel %>%
  distinct(date, .keep_all = TRUE) %>%
  select(date, post_split, all_of(SHOCK), ctrl_afe_w01, ctrl_dollar_w01) %>%
  filter(!is.na(ctrl_afe_w01), !is.na(ctrl_dollar_w01))

path_a <- bind_rows(lapply(
  list(c("AFE dollar",   "ctrl_afe_w01"),
       c("Broad dollar", "ctrl_dollar_w01")),
  function(m) {
    lab <- m[1]; mv <- m[2]

    # --- date-level OLS -------------------------------------------------
    full <- lm(as.formula(sprintf("%s ~ %s * post_split", mv, SHOCK)), data = dates)
    pre  <- lm(as.formula(sprintf("%s ~ %s", mv, SHOCK)),
               data = filter(dates, post_split == 0))
    post <- lm(as.formula(sprintf("%s ~ %s", mv, SHOCK)),
               data = filter(dates, post_split == 1))
    g_lm <- function(mod, term, per) {
      ct <- summary(mod)$coefficients
      if (!term %in% rownames(ct)) return(NULL)
      data.frame(mediator = lab, estimator = "date-level OLS", period = per,
                 term = term, coef = ct[term, 1], se = ct[term, 2],
                 pval = ct[term, 4], n = nobs(mod), row.names = NULL)
    }

    # --- panel, country FE, clustered by date (matches STEP 2 / R/19) ---
    dpanel <- panel %>% filter(!is.na(usd_w01), !is.na(.data[[mv]]))
    full_p <- feols(as.formula(sprintf("%s ~ %s * post_split | country", mv, SHOCK)),
                    cluster = ~date, data = dpanel)
    pre_p  <- feols(as.formula(sprintf("%s ~ %s | country", mv, SHOCK)),
                    cluster = ~date, data = filter(dpanel, post_split == 0))
    post_p <- feols(as.formula(sprintf("%s ~ %s | country", mv, SHOCK)),
                    cluster = ~date, data = filter(dpanel, post_split == 1))
    g_fx <- function(mod, term, per) {
      ct <- as.data.frame(coeftable(mod))
      if (!term %in% rownames(ct)) return(NULL)
      data.frame(mediator = lab, estimator = "panel, country FE, clustered",
                 period = per, term = term, coef = ct[term, "Estimate"],
                 se = ct[term, "Std. Error"],
                 pval = ct[term, grep("^Pr", names(ct))[1]],
                 n = nobs(mod), row.names = NULL)
    }

    bind_rows(
      g_lm(pre,  SHOCK, "pre-split"),
      g_lm(post, SHOCK, "post-split"),
      g_lm(full, paste0(SHOCK, ":post_split"), "interaction"),
      g_fx(pre_p,  SHOCK, "pre-split"),
      g_fx(post_p, SHOCK, "post-split"),
      g_fx(full_p, paste0(SHOCK, ":post_split"), "interaction")
    )
  }
))

cat("\n=== STEP 1 (path a): does the surprise move the dollar factor? ===\n")
cat("    Two estimators, side by side. \"panel, country FE, clustered\" is\n",
    "    the preferred one for the paper -- it matches the specification\n",
    "    used everywhere else in the thesis.\n\n", sep = "")
print(path_a[, c("mediator", "estimator", "period", "coef", "se", "pval", "n")],
      row.names = FALSE, digits = 3)
write_csv(path_a, file.path(paths$out_tables, "mediation_path_a.csv"))

cat("\nDECISION RULE\n",
    "  If the post-split coefficient is significant and signed so that a\n",
    "  tightening surprise strengthens the dollar, mediation is live: the\n",
    "  dollar is on the causal path and controlling for it is over-control.\n",
    "  If it is insignificant and near zero, the dollar cannot be a mediator,\n",
    "  and the attenuation under controls must reflect confounding instead.\n",
    sep = "")

# =============================================================================
# STEP 2. Effect decomposition:  total = direct + (a * b)
# =============================================================================
# Run within the post-split subsample, where the effect lives, so the
# decomposition is a simple two-variable one rather than an interaction.
# For linear OLS on a common sample the identity holds exactly, which is
# checked below.
decomp <- bind_rows(lapply(
  list(c("AFE dollar",   "ctrl_afe_w01"),
       c("Broad dollar", "ctrl_dollar_w01")),
  function(m) {
    lab <- m[1]; mv <- m[2]

    d <- panel %>%
      filter(post_split == 1, !is.na(usd_w01), !is.na(.data[[mv]]))

    m_total  <- feols(as.formula(sprintf("usd_w01 ~ %s | country", SHOCK)),
                      cluster = ~date, data = d)
    m_direct <- feols(as.formula(sprintf("usd_w01 ~ %s + %s | country", SHOCK, mv)),
                      cluster = ~date, data = d)
    m_a      <- feols(as.formula(sprintf("%s ~ %s | country", mv, SHOCK)),
                      cluster = ~date, data = d)

    tot <- as.data.frame(coeftable(m_total))[SHOCK, "Estimate"]
    dir <- as.data.frame(coeftable(m_direct))[SHOCK, "Estimate"]
    a   <- as.data.frame(coeftable(m_a))[SHOCK, "Estimate"]
    b   <- as.data.frame(coeftable(m_direct))[mv, "Estimate"]

    data.frame(
      mediator      = lab,
      total_effect  = tot,
      direct_effect = dir,
      path_a        = a,
      path_b        = b,
      indirect_ab   = a * b,
      identity_gap  = tot - (dir + a * b),
      pct_mediated  = 100 * (a * b) / tot,
      nobs          = nobs(m_total),
      row.names = NULL
    )
  }
))

cat("\n\n=== STEP 2: decomposition of the post-split effect ===\n\n")
print(decomp, row.names = FALSE, digits = 4)
write_csv(decomp, file.path(paths$out_tables, "mediation_decomposition.csv"))

cat("\n  total_effect  = post-split response with no dollar control\n",
    "  direct_effect = post-split response holding the dollar fixed\n",
    "  path_a        = effect of the surprise ON the dollar\n",
    "  path_b        = effect of the dollar on ASEAN returns\n",
    "  indirect_ab   = the share travelling through the dollar\n",
    "  identity_gap  = should be ~0; if not, the decomposition is invalid\n",
    sep = "")

# =============================================================================
# STEP 3. Timing check
# =============================================================================
# A mediator must move no later than the outcome. If the dollar responds only
# in the wider window while ASEAN currencies respond same-day, the ordering is
# wrong for mediation and points back to confounding.
timing <- bind_rows(lapply(c("w0", "w01", "wm11"), function(w) {
  mv <- if (w == "w01") "ctrl_afe_w01" else NA
  # Rebuild the AFE control for this window directly.
  ac <- panel %>% distinct(date) %>% arrange(date) %>%
    left_join(afe, by = "date") %>%
    mutate(r = 100 * (log(dollar_afe) - log(lag(dollar_afe)))) %>%
    mutate(mval = switch(w,
                         w0   = r,
                         w01  = r + lead(r, 1),
                         wm11 = lag(r, 1) + r + lead(r, 1))) %>%
    select(date, mval)

  dd <- panel %>%
    select(date, post_split, all_of(SHOCK)) %>%
    distinct(date, .keep_all = TRUE) %>%
    left_join(ac, by = "date") %>%
    filter(post_split == 1, !is.na(mval))

  mm <- lm(as.formula(sprintf("mval ~ %s", SHOCK)), data = dd)
  ct <- summary(mm)$coefficients
  data.frame(window = w, coef = ct[SHOCK, 1], se = ct[SHOCK, 2],
             pval = ct[SHOCK, 4], n = nobs(mm), row.names = NULL)
}))

cat("\n\n=== STEP 3: dollar response by window, post-split (date-level) ===\n\n")
print(timing, row.names = FALSE, digits = 3)
write_csv(timing, file.path(paths$out_tables, "mediation_timing.csv"))

# =============================================================================
# Caveat, to be carried into the write-up
# =============================================================================
cat("\n\n--- interpretation caveat ---\n")
cat("Mediation analysis identifies the indirect path only under the assumption\n",
    "that nothing unobserved jointly drives the dollar factor and ASEAN returns\n",
    "on announcement days. That assumption is not testable here and is unlikely\n",
    "to hold exactly. Report the decomposition as evidence on which reading is\n",
    "more consistent with the data, not as a causal channel estimate.\n", sep = "")

message("\nSaved: output/tables/mediation_*.csv")
