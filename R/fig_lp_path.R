# =============================================================================
# fig_lp_path.R
# Coefficient-path plot for R/24's forward local projections: cumulative
# [0,+h] response to the Chinese MP shock, h = 0..10, with 90% and 95% bands.
# One panel per shock measure (1Y, 5Y).
#
# Input : output/tables/lp_horizon_response.csv   (from R/24)
# Output: output/figures/fig_lp_coefficient_path.{pdf,png}
# =============================================================================

source("R/00_setup.R")
source("R/fig_theme.R")

lp <- read_csv(file.path(paths$out_tables, "lp_horizon_response.csv"),
               show_col_types = FALSE)

p <- ggplot(lp, aes(x = h, y = coef)) +
  geom_hline(yintercept = 0, colour = "grey50", linetype = "dashed") +
  geom_ribbon(aes(ymin = ci95_lo, ymax = ci95_hi), fill = "#1b9e77", alpha = 0.15) +
  geom_ribbon(aes(ymin = ci90_lo, ymax = ci90_hi), fill = "#1b9e77", alpha = 0.30) +
  geom_line(colour = "#1b9e77", linewidth = 0.7) +
  geom_point(colour = "#1b9e77", size = 1.4) +
  facet_wrap(~ shock, ncol = 2) +
  scale_x_continuous(breaks = 0:10) +
  labs(
    title    = "Cumulative ASEAN-5 FX Response to Chinese MP Shocks, by Horizon",
    subtitle = "Local projection at [0,+h]; country FE, SEs clustered by date; darker band = 90% CI, lighter = 95% CI",
    x        = "Horizon h (trading days after announcement)",
    y        = "Cumulative FX response (%)"
  ) +
  theme_thesis()

save_fig(p, "fig_lp_coefficient_path")
message("Saved fig_lp_coefficient_path to ", paths$out_figures)
