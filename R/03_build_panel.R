# =============================================================================
# 03_build_panel.R
# Reshapes wide FX returns to a long country-date panel, merges the main
# policy shocks onto FX dates, and builds the event-window outcomes.
#
# Input : data-clean/fx_returns_wide.csv
#         data-clean/policy_shocks_main.csv
# Output: data-clean/fx_panel.csv          (long panel: levels + same-day return)
#         data-clean/reg_data0.csv          (panel + shocks + same-day [0] vars)
#         data-clean/reg_data01_main.csv    ([0,+1] outcome, 2008-2020 sample)
#
# On non-event days the shock is coded 0 and event_day = 0, so the panel holds
# both event and non-event days.
# =============================================================================

source("R/00_setup.R")

fx_returns  <- read_csv(file.path(paths$clean, "fx_returns_wide.csv"),
                        show_col_types = FALSE)
policy_main <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"),
                        show_col_types = FALSE)

# --- Reshape to long country-date panel --------------------------------------
fx_panel <- fx_returns %>%
  select(date, idr, myr, php, sgd, thb,
         ret_idr, ret_myr, ret_php, ret_sgd, ret_thb) %>%
  pivot_longer(cols = c(idr, myr, php, sgd, thb),
               names_to = "country", values_to = "fx_lcu_per_usd") %>%
  pivot_longer(cols = c(ret_idr, ret_myr, ret_php, ret_sgd, ret_thb),
               names_to = "return_country", values_to = "fx_return") %>%
  mutate(return_country = sub("^ret_", "", return_country)) %>%
  filter(country == return_country) %>%
  select(date, country, fx_lcu_per_usd, fx_return) %>%
  arrange(country, date)

write_csv(fx_panel, file.path(paths$clean, "fx_panel.csv"))

# --- Merge policy shocks onto FX dates ---------------------------------------
reg_data0 <- fx_panel %>%
  left_join(
    policy_main %>%
      select(date, shock_1y, shock_5y,
             isdRRR, isdRevrepo, isdLDR, isdMLF, isMPR, isFX, isTMLF),
    by = "date"
  ) %>%
  mutate(
    event_day = if_else(is.na(shock_1y), 0, 1),
    shock_1y  = if_else(is.na(shock_1y), 0, shock_1y),
    shock_5y  = if_else(is.na(shock_5y), 0, shock_5y),
    across(c(isdRRR, isdRevrepo, isdLDR, isdMLF, isMPR, isFX, isTMLF),
           ~ if_else(is.na(.x), FALSE, .x))
  )

write_csv(reg_data0, file.path(paths$clean, "reg_data0.csv"))

# Merge checks
message("Distinct main shock dates: ", n_distinct(policy_main$date))
message("Of those, matched to an FX trading day: ",
        sum(unique(policy_main$date) %in% unique(fx_panel$date)))

# --- [0,+1] event-window outcome ---------------------------------------------
reg_data01 <- reg_data0 %>%
  group_by(country) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    fx_return_lead1 = lead(fx_return),
    fx_return_01    = fx_return + fx_return_lead1
  ) %>%
  ungroup()

# --- Restrict to main sample -------------------------------------------------
reg_data01_main <- reg_data01 %>%
  filter(date >= SAMPLE_START, date <= SAMPLE_END)

write_csv(reg_data01_main, file.path(paths$clean, "reg_data01_main.csv"))
message("Main sample rows (", SAMPLE_START, " to ", SAMPLE_END, "): ",
        nrow(reg_data01_main))
