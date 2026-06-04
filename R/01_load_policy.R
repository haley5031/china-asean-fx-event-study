# =============================================================================
# 01_load_policy.R
# Reads the raw Chinese monetary policy shock series and writes the two
# cleaned policy files used downstream:
#   - policy_shocks_main.csv   (isMain  == TRUE)
#   - policy_shocks_broad.csv  (isBroad == TRUE)
#
# Input : data-raw/policy/china_mpshocks.csv   (never modified)
# Output: data-clean/policy_shocks_main.csv
#         data-clean/policy_shocks_broad.csv
# =============================================================================

source("R/00_setup.R")

# Read date/datetime as plain text first, then parse explicitly. The raw file
# is stored in US M/D/YY format (e.g. "8/19/06", "8/18/06 17:18") because it was
# saved through Excel. Parsing the format explicitly makes this robust whether
# the file is in M/D/YY or ISO YYYY-MM-DD, so a future re-save can't break it.
policy <- read_csv(
  file.path(paths$raw_policy, "china_mpshocks.csv"),
  col_types = cols(date = col_character(), datetime = col_character(),
                    .default = col_guess())
) %>%
  mutate(
    date     = parse_date_robust(date),
    datetime = parse_datetime_robust(datetime)
  )

# Sanity check: no dates should fail to parse.
n_bad <- sum(is.na(policy$date))
if (n_bad > 0) stop(n_bad, " date value(s) failed to parse in china_mpshocks.csv")

# --- Main shocks -------------------------------------------------------------
policy_main <- policy %>%
  filter(isMain == TRUE) %>%
  arrange(date, datetime)

write_csv(policy_main, file.path(paths$clean, "policy_shocks_main.csv"))
message("Saved ", nrow(policy_main), " main shocks to data-clean/policy_shocks_main.csv")

# --- Broad shocks (robustness) ----------------------------------------------
policy_broad <- policy %>%
  filter(isBroad == TRUE) %>%
  arrange(date, datetime)

write_csv(policy_broad, file.path(paths$clean, "policy_shocks_broad.csv"))
message("Saved ", nrow(policy_broad), " broad shocks to data-clean/policy_shocks_broad.csv")

# --- Quick integrity check ---------------------------------------------------
# Reconcile the event-date count reported in the write-up against the data.
message("Unique main event dates: ", n_distinct(policy_main$date))
