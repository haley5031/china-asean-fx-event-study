# =============================================================================
# fig_shock_stem.R
# Figure 2: Chinese monetary policy surprises, main event set, shock_1y in
# basis points. Stem plot: one vertical segment per announcement date.
#
# Input : data-clean/policy_shocks_main.csv
# Output: output/figures/fig2_shock_stem.{pdf,png}
# =============================================================================

source("R/00_setup.R")
source("R/fig_theme.R")

shocks <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"), show_col_types = FALSE) %>%
  arrange(date) %>%
  mutate(shock_1y_bps = shock_1y * 100)

p <- ggplot(shocks, aes(x = date, y = shock_1y_bps)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_segment(aes(xend = date, yend = 0), colour = ACCENT_COLOR, linewidth = 0.4) +
  geom_point(colour = ACCENT_COLOR, size = 1.3) +
  scale_x_date_thesis() +
  scale_y_continuous(labels = label_bps_thesis) +
  labs(
    title    = "Chinese Monetary Policy Surprises",
    subtitle = "1-year swap shock, main event set (n = 102), basis points",
    x        = NULL,
    y        = "Shock (bps)"
  ) +
  theme_thesis()

save_fig(p, "fig2_shock_stem")
message("Saved fig2_shock_stem to ", paths$out_figures)
