# =============================================================================
# 27_instrument_type_excl_nov2008.R
# 26 Nov 2008 is a joint RRR + benchmark-rate (LDR) cut -- both legs are
# QUANTITY-type flags in R/16/R/17's classification (isdRRR & isdLDR both
# TRUE, isdRevrepo & isdMLF both FALSE), so it sits entirely in the
# quantity partition, not split across price/quantity. Confirmed directly
# from the flags below, not assumed.
#
# R/17's Table 11 / Equation 9 specification reports, for each instrument
# type (price, quantity): the pre-split response, the post-split response,
# and the post-minus-pre difference -- six cells. This script re-estimates
# all six with 26 Nov 2008 excluded, reported alongside the original (all
# events) six, so the two can be read side by side rather than inferred.
#
# Input : data-clean/reg_data_ext_main.csv, data-clean/policy_shocks_main.csv
# Output: output/tables/instrument_table11_excl_nov2008.csv
#         output/tables/instrument_nov2008_classification.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

policy <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"),
                   show_col_types = FALSE) %>%
  select(date, shock_1y, isdRRR, isdRevrepo, isdLDR, isdMLF)

SHOCK <- "shock_1y"
nov2008 <- as.Date("2008-11-26")

# =============================================================================
# 0. Confirm the classification of 26 Nov 2008 directly from the flags
# =============================================================================
nov_flags <- policy %>% filter(date == nov2008)
stopifnot(nrow(nov_flags) == 1)

nov_class <- nov_flags %>%
  mutate(
    quantity = isdRRR | isdLDR,
    price    = isdRevrepo | isdMLF,
    type = case_when(
      quantity & !price ~ "quantity",
      price & !quantity ~ "price",
      quantity & price  ~ "mixed",
      TRUE              ~ "unflagged"
    )
  )

cat("\n=== 0. 26 Nov 2008 classification (Das & Song instrument flags) ===\n\n")
print(as.data.frame(nov_class), row.names = FALSE)
write_csv(nov_class, file.path(paths$out_tables, "instrument_nov2008_classification.csv"))

if (nov_class$type == "mixed") {
  cat("\nAMBIGUOUS: this event is flagged on BOTH a price and a quantity\n",
      "indicator and is classified 'mixed' -- it does not sit cleanly in\n",
      "either partition. Disclose this rather than assigning it to one side.\n", sep = "")
} else {
  cat(sprintf(
    "\nNOT ambiguous: isdRRR = %s, isdLDR = %s (both quantity flags); isdRevrepo = %s,\n",
    nov_class$isdRRR, nov_class$isdLDR, nov_class$isdRevrepo))
  cat(sprintf(
    "isdMLF = %s (both price flags, both FALSE). Classified '%s' cleanly -- it is\n",
    nov_class$isdMLF, nov_class$type))
  cat("flagged on TWO quantity indicators simultaneously (a joint RRR + LDR move),\n",
      "which is a different thing from being ambiguous between price and quantity:\n",
      "both indicators that fire agree on the same side of the partition.\n", sep = "")
}

# =============================================================================
# 1. Classify every event, build the type-specific shock variables (same
#    construction as R/17, so the reparameterisation identity still holds)
# =============================================================================
build_pnl <- function(drop_date = NULL) {
  events <- policy %>%
    filter(date >= SAMPLE_START, date <= SAMPLE_END, .data[[SHOCK]] != 0) %>%
    mutate(
      quantity = isdRRR | isdLDR,
      price    = isdRevrepo | isdMLF,
      type = case_when(
        quantity & !price ~ "quantity",
        price & !quantity ~ "price",
        quantity & price  ~ "mixed",
        TRUE              ~ "unflagged"
      )
    )

  pnl <- panel %>%
    left_join(select(events, date, type), by = "date") %>%
    mutate(
      type        = coalesce(type, "none"),
      shock_price = if_else(type == "price",    .data[[SHOCK]], 0),
      shock_qty   = if_else(type == "quantity", .data[[SHOCK]], 0)
    )

  if (!is.null(drop_date)) pnl <- filter(pnl, date != drop_date)
  pnl
}

# =============================================================================
# 2. Six cells x {all events, excl. 26 Nov 2008}: price/quantity x
#    {pre-split level, post-split level, post-minus-pre difference}
# =============================================================================
six_cells <- function(pnl, label) {
  pre  <- filter(pnl, post_split == 0)
  post <- filter(pnl, post_split == 1)

  m_pre  <- feols(usd_w01 ~ shock_price + shock_qty | country, cluster = ~date, data = pre)
  m_post <- feols(usd_w01 ~ shock_price + shock_qty | country, cluster = ~date, data = post)
  m_int  <- feols(usd_w01 ~ shock_price + shock_qty + shock_price:post_split +
                            shock_qty:post_split + post_split | country,
                  cluster = ~date, data = pnl)

  bind_rows(
    coef_row(m_pre,  "shock_price", "price",    extra = list(spec = label, column = "pre-split level")),
    coef_row(m_post, "shock_price", "price",    extra = list(spec = label, column = "post-split level")),
    coef_row(m_int,  "shock_price:post_split", "price", extra = list(spec = label, column = "post minus pre")),
    coef_row(m_pre,  "shock_qty",  "quantity", extra = list(spec = label, column = "pre-split level")),
    coef_row(m_post, "shock_qty",  "quantity", extra = list(spec = label, column = "post-split level")),
    coef_row(m_int,  "shock_qty:post_split",  "quantity", extra = list(spec = label, column = "post minus pre"))
  )
}

pnl_all     <- build_pnl(NULL)
pnl_excl    <- build_pnl(nov2008)

# Partition check on both samples before relying on the reparameterisation.
for (nm in c("pnl_all", "pnl_excl")) {
  d <- get(nm)
  gap <- max(abs(d[[SHOCK]] - (d$shock_price + d$shock_qty)), na.rm = TRUE)
  cat(sprintf("Partition check (%s): max|shock - (price + qty)| = %.2e\n", nm, gap))
  if (gap > 1e-12) stop("Event types do not partition the shock in ", nm)
}

table11 <- bind_rows(
  six_cells(pnl_all,  "all events"),
  six_cells(pnl_excl, "excl. 26 Nov 2008")
) %>%
  mutate(column = factor(column, levels = c("pre-split level", "post-split level", "post minus pre"))) %>%
  arrange(term, column, factor(spec, levels = c("all events", "excl. 26 Nov 2008")))

cat("\n\n=== 1. Table 11 / Equation 9, all six cells, with and without 26 Nov 2008 ===\n\n")
print(table11[, c("term", "column", "spec", "coef", "se", "pval", "nobs")],
      row.names = FALSE, digits = 3)
write_csv(table11, file.path(paths$out_tables, "instrument_table11_excl_nov2008.csv"))

qty_pre <- table11 %>% filter(term == "quantity", column == "pre-split level")
qty_dif <- table11 %>% filter(term == "quantity", column == "post minus pre")

cat("\n\n--- Read: the quantity leg the user flagged ---\n")
cat(sprintf("quantity, pre-split level : all events = %+.3f (p=%.3f)  ->  excl. Nov 2008 = %+.3f (p=%.3f)\n",
            qty_pre$coef[1], qty_pre$pval[1], qty_pre$coef[2], qty_pre$pval[2]))
cat(sprintf("quantity, post minus pre  : all events = %+.3f (p=%.3f)  ->  excl. Nov 2008 = %+.3f (p=%.3f)\n",
            qty_dif$coef[1], qty_dif$pval[1], qty_dif$coef[2], qty_dif$pval[2]))
cat(sprintf(
  "\n%s\n",
  if (qty_dif$pval[2] >= 0.05 && qty_dif$pval[1] < 0.05)
    "The quantity-response CHANGE across the split does NOT survive dropping 26 Nov 2008: the same single date drives Section 4.7's quantity-instrument claim as drives R1's own response."
  else
    "The quantity-response CHANGE across the split survives dropping 26 Nov 2008 (still significant, or was already not significant either way)."
))

message("\nSaved: output/tables/instrument_table11_excl_nov2008.csv, instrument_nov2008_classification.csv")
