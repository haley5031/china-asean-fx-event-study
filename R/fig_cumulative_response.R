# =============================================================================
# fig_cumulative_response.R
# Figure 3: average cumulative ASEAN-5 FX response around Chinese monetary
# policy shocks, pooled across countries and the 65 in-sample main events.
# Cumulative return is normalized to zero at t-1 (the last pre-event day);
# the [0,+1] event window is highlighted.
#
# Input : data-clean/fx_panel.csv
#         data-clean/reg_data01_main.csv   (defines the in-sample event dates)
# Output: output/figures/fig3_cumulative_response.{pdf,png}
# =============================================================================

source("R/00_setup.R")
source("R/fig_theme.R")

fx_panel <- read_csv(file.path(paths$clean, "fx_panel.csv"), show_col_types = FALSE)
reg      <- read_csv(file.path(paths$clean, "reg_data01_main.csv"), show_col_types = FALSE)

event_dates <- sort(unique(reg$date[reg$event_day == 1]))
dates_all   <- sort(unique(fx_panel$date))
event_idx   <- match(event_dates, dates_all)

REL_WINDOW <- -3:3
HIGHLIGHT  <- c(0, 1)

# One row per event x relative offset x country, with the trading-day-offset
# target date resolved via position in dates_all (not calendar arithmetic, so
# weekends/holidays are skipped consistently with the rest of the panel).
event_grid <- tidyr::expand_grid(
  event_id = seq_along(event_dates),
  rel_day  = REL_WINDOW,
  country  = ASEAN5
) %>%
  mutate(
    event_date  = event_dates[event_id],
    target_idx  = event_idx[event_id] + rel_day,
    target_date = dates_all[target_idx]
  ) %>%
  filter(!is.na(target_date)) %>%
  left_join(fx_panel %>% select(date, country, fx_return),
            by = c("target_date" = "date", "country" = "country"))

car_df <- event_grid %>%
  filter(!is.na(fx_return)) %>%
  arrange(event_id, country, rel_day) %>%
  group_by(event_id, country) %>%
  mutate(cum = cumsum(fx_return),
         car = cum - cum[rel_day == -1][1]) %>%
  ungroup()

car_summary <- car_df %>%
  group_by(rel_day) %>%
  summarise(
    mean_car = mean(car, na.rm = TRUE),
    se       = sd(car, na.rm = TRUE) / sqrt(sum(!is.na(car))),
    n        = sum(!is.na(car)),
    .groups  = "drop"
  ) %>%
  mutate(lower = mean_car - 1.96 * se,
         upper = mean_car + 1.96 * se)

p <- ggplot(car_summary, aes(x = rel_day, y = mean_car)) +
  annotate("rect", xmin = HIGHLIGHT[1] - 0.5, xmax = HIGHLIGHT[2] + 0.5,
           ymin = -Inf, ymax = Inf, fill = "#1b9e77", alpha = 0.10) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#1b9e77", alpha = 0.25) +
  geom_line(colour = "#1b9e77", linewidth = 0.7) +
  geom_point(colour = "#1b9e77", size = 1.6) +
  scale_x_continuous(breaks = REL_WINDOW) +
  labs(
    title    = "Average Cumulative ASEAN-5 FX Response to Chinese MP Shocks",
    subtitle = "Pooled across countries and main-sample events; normalized to zero at t-1; shaded band is 95% CI",
    x        = "Trading days relative to announcement (t = 0)",
    y        = "Cumulative FX return (%)",
    caption  = "Shaded rectangle marks the [0,+1] event window."
  ) +
  theme_thesis()

save_fig(p, "fig3_cumulative_response")
message("Saved fig3_cumulative_response to ", paths$out_figures)
