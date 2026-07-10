# =============================================================================
# fig_fx_series.R
# Figure 1: ASEAN-5 exchange rates indexed to 100 at the first 2008 trading
# day, local currency per USD (a rise = depreciation against the dollar).
#
# Input : data-clean/fx_asean5_clean.csv
# Output: output/figures/fig1_fx_indexed.{pdf,png}
# =============================================================================

source("R/00_setup.R")
source("R/fig_theme.R")
library(tidyr)

fx <- read_csv(file.path(paths$clean, "fx_asean5_clean.csv"), show_col_types = FALSE) %>%
  arrange(date)

base_date <- min(fx$date[fx$date >= as.Date("2008-01-01")])

fx_idx <- fx %>%
  # Index each country to its own first non-missing value on/after base_date,
  # rather than one shared calendar date: a country-specific holiday on
  # base_date (e.g. Thailand on 2008-01-02) would otherwise leave that
  # column's base value NA and silently drop the whole series from the plot.
  mutate(across(all_of(ASEAN5), ~ .x / .x[date >= base_date & !is.na(.x)][1] * 100)) %>%
  pivot_longer(cols = all_of(ASEAN5), names_to = "country", values_to = "index") %>%
  mutate(country = factor(country_labels[country], levels = country_labels[ASEAN5]))

p <- ggplot(fx_idx, aes(x = date, y = index, colour = country)) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = country_palette) +
  scale_x_date_thesis() +
  labs(
    title    = "ASEAN-5 Exchange Rates Against the U.S. Dollar",
    subtitle = sprintf("Indexed to 100 on %s (first 2008 trading day); a rise is a depreciation",
                        format(base_date, "%d %b %Y")),
    x        = NULL,
    y        = "Index (base = 100)"
  ) +
  theme_thesis()

save_fig(p, "fig1_fx_indexed")
message("Saved fig1_fx_indexed to ", paths$out_figures)
