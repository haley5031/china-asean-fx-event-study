# =============================================================================
# 16_instrument_mix.R
# Das and Song (2023, sec. 4.2.1) note that the benchmark deposit and lending
# rates (LDR) "have not been changed since 2015 and are in the process of
# gradually being phased out but were in use in the earlier part of our
# sample", while the MLF "has become the main policy instrument by which to
# influence bank lending rates since mid-2019".
#
# This implies the shock series is not homogeneous across the 2015 split: the
# MIX OF POLICY INSTRUMENTS generating the surprises changes. Pre-2015 events
# lean on administered and quantity-based tools (LDR, RRR); post-2015 events
# lean on market-based rates (7-day reverse repo, MLF). That is a substantive
# mechanism for regime dependence rather than a coincidence of dates.
#
# This script does two things.
#
#   1. Documents the composition change directly from the event flags.
#   2. Tests whether the response differs by instrument TYPE, pooling across
#      the full sample.
#
# IMPORTANT CAVEAT, built into the output. Instrument type and period are
# heavily confounded by construction: LDR events essentially stop after 2015
# and MLF events essentially start in 2019. The cross-tabulation is printed so
# that the overlap is visible rather than assumed away. Where a type-by-period
# cell is thin or empty, the two explanations cannot be separated, and the
# write-up must say so.
#
# Input : data-clean/reg_data_ext_main.csv
#         data-clean/policy_shocks_main.csv   (instrument flags)
# Output: output/tables/instrument_*.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

policy <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"),
                   show_col_types = FALSE) %>%
  select(date, shock_1y, isdRRR, isdRevrepo, isdLDR, isdMLF)

SHOCK <- "shock_1y"

# --- Classify each event -----------------------------------------------------
# Quantity / administered : RRR, benchmark deposit and lending rates
# Price / market-based    : 7-day reverse repo, MLF
# An event can trip more than one flag (e.g. a simultaneous RRR and LDR move),
# so "mixed" is retained as its own category rather than forced into one side.
events <- policy %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END, shock_1y != 0) %>%
  mutate(
    quantity = isdRRR | isdLDR,
    price    = isdRevrepo | isdMLF,
    type = case_when(
      quantity & !price ~ "quantity (RRR/LDR)",
      price & !quantity ~ "price (repo/MLF)",
      quantity & price  ~ "mixed",
      TRUE              ~ "unflagged"
    ),
    period = if_else(date >= SPLIT_DATE, "post", "pre")
  )

# --- 1a. Composition by period ----------------------------------------------
comp <- events %>%
  count(period, type) %>%
  tidyr::pivot_wider(names_from = period, values_from = n, values_fill = 0)

cat("\n=== 1a. Instrument mix of nonzero surprises, by period ===\n\n")
print(as.data.frame(comp), row.names = FALSE)
write_csv(comp, file.path(paths$out_tables, "instrument_composition.csv"))

cat("\nThis is the confounding, stated plainly. If one instrument type is\n",
    "essentially absent from one side of the split, 'the response changed\n",
    "after 2015' and 'the response differs by instrument' are the same\n",
    "statement expressed two ways, and neither can be credited over the other.\n",
    sep = "")

# --- 1b. Individual flags, and surprise size --------------------------------
flags <- events %>%
  group_by(period) %>%
  summarise(
    n_events  = n(),
    RRR       = sum(isdRRR),
    LDR       = sum(isdLDR),
    RevRepo   = sum(isdRevrepo),
    MLF       = sum(isdMLF),
    mean_abs  = mean(abs(.data[[SHOCK]])),
    max_abs   = max(abs(.data[[SHOCK]])),
    .groups   = "drop"
  )

cat("\n\n=== 1b. Individual instrument flags and surprise size ===\n\n")
print(as.data.frame(flags), row.names = FALSE, digits = 3)
write_csv(flags, file.path(paths$out_tables, "instrument_flags.csv"))

# --- 1c. Timing of each instrument ------------------------------------------
span <- events %>%
  tidyr::pivot_longer(c(isdRRR, isdLDR, isdRevrepo, isdMLF),
                      names_to = "instrument", values_to = "used") %>%
  filter(used) %>%
  group_by(instrument) %>%
  summarise(n = n(), first = min(date), last = max(date), .groups = "drop")

cat("\n\n=== 1c. First and last use of each instrument in the sample ===\n\n")
print(as.data.frame(span), row.names = FALSE)
write_csv(span, file.path(paths$out_tables, "instrument_span.csv"))

# =============================================================================
# 2. Does the response differ by instrument type?
# =============================================================================
# Build type-specific shock variables: the surprise value on events of that
# type, zero elsewhere. This keeps the panel intact and lets both enter the
# same regression, so the difference is estimated rather than eyeballed across
# two separate models.
type_map <- events %>%
  select(date, type)

pnl <- panel %>%
  left_join(type_map, by = "date") %>%
  mutate(
    type          = coalesce(type, "none"),
    shock_price   = if_else(type == "price (repo/MLF)",   .data[[SHOCK]], 0),
    shock_qty     = if_else(type == "quantity (RRR/LDR)", .data[[SHOCK]], 0),
    shock_mixed   = if_else(type == "mixed",              .data[[SHOCK]], 0)
  )

m_type <- feols(usd_w01 ~ shock_price + shock_qty + shock_mixed | country,
                cluster = ~date, data = pnl)

cat("\n\n=== 2a. Response by instrument type, full sample ([0,+1], USD) ===\n")
print(etable(list("By instrument type" = m_type),
             cluster = ~date, digits = 3, fitstat = c("n", "r2", "wr2")))

# Formal test that the price and quantity responses are equal.
cat("\n--- Wald test: price response = quantity response ---\n")
print(wald(m_type, "shock_price|shock_qty"))
cat("\n(If the two cannot be distinguished, instrument type does not by itself\n",
    "explain the regime result.)\n", sep = "")

# --- 2b. Type response within each period -----------------------------------
# Where a cell is empty this will drop the term; that absence is the point.
for (per in c("pre", "post")) {
  dd <- if (per == "pre") filter(pnl, post_split == 0) else filter(pnl, post_split == 1)
  n_by_type <- events %>%
    filter(period == per) %>% count(type)
  cat("\n\n=== 2b. ", per, "-split: response by instrument type ===\n", sep = "")
  cat("Events available:\n")
  print(as.data.frame(n_by_type), row.names = FALSE)
  m <- tryCatch(
    feols(usd_w01 ~ shock_price + shock_qty + shock_mixed | country,
          cluster = ~date, data = dd),
    error = function(e) NULL
  )
  if (!is.null(m)) print(etable(list(m), cluster = ~date, digits = 3))
  else cat("  (not estimable in this subsample)\n")
}

message("\nSaved: output/tables/instrument_*.csv")
