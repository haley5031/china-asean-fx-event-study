# =============================================================================
# 17_instrument_tests.R
# Follow-up to R/16, fixing one error and testing three claims that the R/16
# output suggests but does not establish.
#
# THE ERROR. R/16 used wald(m, "shock_price|shock_qty"), which tests the JOINT
# NULLITY of the two coefficients -- whether both are zero -- not whether they
# are EQUAL to each other. The reported p = 0.27 therefore says nothing about
# the price-versus-quantity difference. The correct test needs a
# reparameterisation, implemented below.
#
# THE REPARAMETERISATION. Because the event types partition the nonzero-surprise
# dates exactly (every event is either price-based or quantity-based; no mixed
# or unflagged events occur in the estimation sample),
#
#       shock_1y  =  shock_price + shock_qty
#
# identically. Estimating
#
#       y  ~  shock_1y + shock_price
#
# therefore gives a coefficient on shock_1y equal to the QUANTITY response, and
# a coefficient on shock_price equal to the DIFFERENCE (price minus quantity),
# with a standard error attached. The difference is estimated directly rather
# than inferred by comparing two separate coefficients -- which is the
# difference-of-significance error the design elsewhere is careful to avoid.
#
# THREE CLAIMS TESTED
#   1. Post-split, does the price response differ significantly from the
#      quantity response?
#   2. Does the price response differ significantly between periods?
#   3. Does the post-split price result survive dropping any single one of the
#      11 announcements identifying it?
#
# Input : data-clean/reg_data_ext_main.csv, data-clean/policy_shocks_main.csv
# Output: output/tables/instrument_tests_*.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

policy <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"),
                   show_col_types = FALSE) %>%
  select(date, shock_1y, isdRRR, isdRevrepo, isdLDR, isdMLF)

SHOCK <- "shock_1y"

events <- policy %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END, shock_1y != 0) %>%
  mutate(
    quantity = isdRRR | isdLDR,
    price    = isdRevrepo | isdMLF,
    type = case_when(
      quantity & !price ~ "quantity",
      price & !quantity ~ "price",
      quantity & price  ~ "mixed",
      TRUE              ~ "unflagged"
    ),
    period = if_else(date >= SPLIT_DATE, "post", "pre")
  )

pnl <- panel %>%
  left_join(select(events, date, type), by = "date") %>%
  mutate(
    type        = coalesce(type, "none"),
    shock_price = if_else(type == "price",    .data[[SHOCK]], 0),
    shock_qty   = if_else(type == "quantity", .data[[SHOCK]], 0)
  )

# Verify the partition before relying on it.
gap <- pnl %>%
  summarise(m = max(abs(.data[[SHOCK]] - (shock_price + shock_qty)), na.rm = TRUE)) %>%
  pull(m)
cat(sprintf("\nPartition check: max|shock - (price + qty)| = %.2e\n", gap))
if (gap > 1e-12)
  stop("Event types do not partition the shock; the reparameterisation is invalid.")

pre  <- filter(pnl, post_split == 0)
post <- filter(pnl, post_split == 1)

# =============================================================================
# CLAIM 1. Price vs quantity, within each period
# =============================================================================
cat("\n\n=== 1. Price minus quantity response, by period ===\n")
cat("    Coefficient on shock_1y  = quantity response\n")
cat("    Coefficient on shock_price = price MINUS quantity (the difference)\n")

m_diff_full <- feols(usd_w01 ~ shock_1y + shock_price | country,
                     cluster = ~date, data = pnl)
m_diff_pre  <- feols(usd_w01 ~ shock_1y + shock_price | country,
                     cluster = ~date, data = pre)
m_diff_post <- feols(usd_w01 ~ shock_1y + shock_price | country,
                     cluster = ~date, data = post)

print(etable(list("full sample" = m_diff_full,
                  "pre-split"   = m_diff_pre,
                  "post-split"  = m_diff_post),
             cluster = ~date, digits = 3, fitstat = c("n", "r2")))

claim1 <- bind_rows(
  coef_row(m_diff_full, "shock_price", "price - quantity", extra = list(period = "full")),
  coef_row(m_diff_pre,  "shock_price", "price - quantity", extra = list(period = "pre")),
  coef_row(m_diff_post, "shock_price", "price - quantity", extra = list(period = "post"))
)
cat("\n")
print(claim1[, c("period", "term", "coef", "se", "pval")], row.names = FALSE, digits = 3)
write_csv(claim1, file.path(paths$out_tables, "instrument_tests_claim1.csv"))

cat("\nIf the post-split difference is not significant, the correct statement is\n",
    "that the price response is significantly different from ZERO while the\n",
    "quantity response is not -- which is NOT the same as the two being\n",
    "significantly different from each other.\n", sep = "")

# =============================================================================
# CLAIM 2. Price response across periods
# =============================================================================
# Full interaction. shock_price:post_split is the change in the price response
# across the split; shock_qty:post_split the change in the quantity response.
m_int <- feols(
  usd_w01 ~ shock_price + shock_qty + shock_price:post_split +
            shock_qty:post_split + post_split | country,
  cluster = ~date, data = pnl
)

cat("\n\n=== 2. Change in each instrument's response across the split ===\n")
print(etable(list("interacted" = m_int), cluster = ~date, digits = 3,
             fitstat = c("n", "r2")))

claim2 <- bind_rows(
  coef_row(m_int, "shock_price",            "price, pre-split"),
  coef_row(m_int, "shock_qty",              "quantity, pre-split"),
  coef_row(m_int, "shock_price:post_split", "price: post minus pre"),
  coef_row(m_int, "shock_qty:post_split",   "quantity: post minus pre")
)
cat("\n")
print(claim2[, c("term", "coef", "se", "pval")], row.names = FALSE, digits = 3)
write_csv(claim2, file.path(paths$out_tables, "instrument_tests_claim2.csv"))

cat("\nThe row that licenses a regime claim is 'price: post minus pre'. If it is\n",
    "insignificant, the data show a price response detectable after 2015 and\n",
    "not before, without establishing that the response CHANGED.\n", sep = "")

# =============================================================================
# CLAIM 3. Influence: the 11 post-split price events
# =============================================================================
post_price <- events %>%
  filter(period == "post", type == "price") %>%
  arrange(date)

cat("\n\n=== 3. Leave-one-out over the ", nrow(post_price),
    " post-split price events ===\n", sep = "")
cat("\nThe events identifying the result:\n")
print(as.data.frame(
  post_price %>%
    mutate(instrument = if_else(isdMLF, "MLF", "reverse repo")) %>%
    select(date, shock_1y, instrument)),
  row.names = FALSE, digits = 3)

base_m <- feols(usd_w01 ~ shock_price + shock_qty | country,
                cluster = ~date, data = post)
base_c <- as.data.frame(coeftable(base_m))["shock_price", "Estimate"]
base_p <- as.data.frame(coeftable(base_m))["shock_price",
                                           grep("^Pr", names(as.data.frame(coeftable(base_m))))[1]]

loo <- bind_rows(lapply(seq_len(nrow(post_price)), function(i) {
  d  <- post_price$date[i]
  m  <- feols(usd_w01 ~ shock_price + shock_qty | country,
              cluster = ~date, data = filter(post, date != d))
  ct <- as.data.frame(coeftable(m))
  data.frame(dropped = as.character(d),
             shock   = post_price$shock_1y[i],
             coef    = ct["shock_price", "Estimate"],
             se      = ct["shock_price", "Std. Error"],
             pval    = ct["shock_price", grep("^Pr", names(ct))[1]],
             row.names = NULL, stringsAsFactors = FALSE)
}))

cat(sprintf("\nWith all events: %.3f (p = %.4f)\n\n", base_c, base_p))
print(loo[order(loo$coef), ], row.names = FALSE, digits = 3)
write_csv(loo, file.path(paths$out_tables, "instrument_tests_loo.csv"))

cat(sprintf("\nRange: [%.3f, %.3f].  p < 0.05 in %d of %d drops; p < 0.10 in %d of %d.\n",
            min(loo$coef), max(loo$coef),
            sum(loo$pval < 0.05), nrow(loo),
            sum(loo$pval < 0.10), nrow(loo)))
cat("\nWith an identifying set this small, a coefficient that survives every\n",
    "drop is reportable as a finding; one that depends on a single event is\n",
    "reportable only as a description of those events.\n", sep = "")

# --- Also: does it survive the dollar control? -------------------------------
if ("ctrl_dollar_w01" %in% names(post)) {
  m_ctrl <- feols(usd_w01 ~ shock_price + shock_qty + ctrl_dollar_w01 | country,
                  cluster = ~date, data = post)
  cat("\n\n=== 3b. Post-split price response, with the dollar control ===\n")
  print(etable(list("no control" = base_m, "+ broad dollar" = m_ctrl),
               cluster = ~date, digits = 3, fitstat = c("n", "r2", "wr2")))
  cat("\nGiven the mediation result, attenuation here is expected and is not\n",
      "evidence against the finding -- the dollar sits on the causal path.\n", sep = "")
}

message("\nSaved: output/tables/instrument_tests_*.csv")
