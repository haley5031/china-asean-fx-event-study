# =============================================================================
# fig_attrition_funnel.R
# Sample-construction attrition funnel: main event dates -> matched to the FX
# panel -> inside the 2008-2020 sample window -> nonzero 1Y surprise.
# Counts are computed from the cleaned data, not hard-coded.
#
# Input : data-clean/policy_shocks_main.csv
#         data-clean/fx_panel.csv
#         data-clean/reg_data01_main.csv
# Output: output/figures/fig_attrition_funnel.{pdf,png}
# =============================================================================

source("R/00_setup.R")
source("R/fig_theme.R")

policy_main <- read_csv(file.path(paths$clean, "policy_shocks_main.csv"), show_col_types = FALSE)
fx_panel    <- read_csv(file.path(paths$clean, "fx_panel.csv"),           show_col_types = FALSE)
reg         <- read_csv(file.path(paths$clean, "reg_data01_main.csv"),    show_col_types = FALSE)

n_main      <- n_distinct(policy_main$date)
n_matched   <- sum(unique(policy_main$date) %in% unique(fx_panel$date))
n_in_window <- n_distinct(reg$date[reg$event_day == 1])
n_nonzero   <- n_distinct(reg$date[reg$event_day == 1 & reg$shock_1y != 0])

funnel <- data.frame(
  step  = c("Main event dates", "On FX trading day", "2008-2020 sample window",
            "Nonzero 1Y surprise"),
  n     = c(n_main, n_matched, n_in_window, n_nonzero),
  drop  = c(NA_character_,
            sprintf("-%d: non-trading day", n_main - n_matched),
            sprintf("-%d: outside 2008-2020", n_matched - n_in_window),
            sprintf("-%d: zero 1Y surprise", n_in_window - n_nonzero))
)
funnel$step <- factor(funnel$step, levels = rev(funnel$step))

p <- ggplot(funnel, aes(x = n, y = step)) +
  geom_col(fill = ACCENT_COLOR, width = 0.6) +
  geom_text(aes(label = n), hjust = -0.3, size = 3.6, family = FIG_FONT) +
  geom_text(aes(label = drop, x = max(funnel$n) * 1.18), hjust = 0,
            colour = "grey40", size = 3, family = FIG_FONT) +
  scale_x_continuous(limits = c(0, max(funnel$n) * 1.55), expand = c(0, 0)) +
  labs(
    title    = "Sample-Construction Attrition Funnel",
    subtitle = "Main event dates narrowed to the estimation sample",
    x        = "Number of event dates",
    y        = NULL
  ) +
  theme_thesis() +
  theme(panel.grid.major.y = element_blank())

save_fig(p, "fig_attrition_funnel")
message("Saved fig_attrition_funnel to ", paths$out_figures,
        " (", n_main, " -> ", n_matched, " -> ", n_in_window, " -> ", n_nonzero, ")")
