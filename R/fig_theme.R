# =============================================================================
# fig_theme.R
# Shared styling for every thesis figure: font, base size, palette, and a
# save_fig() helper so all figures share dimensions and output format.
# Sourced by each fig_*.R script after R/00_setup.R.
# =============================================================================

library(ggplot2)
library(scales)

FIG_FONT      <- "sans"
FIG_BASE_SIZE <- 11
FIG_WIDTH     <- 7.5
FIG_HEIGHT    <- 4.5

# ASEAN-5 country labels and a consistent colorblind-friendly palette, keyed
# by the lower-case currency codes used throughout the panel. Colors are
# RColorBrewer::brewer.pal(8, "Dark2"); every figure that draws more than one
# country must map colour/fill to this vector so a country is always the same
# hue everywhere it appears.
country_labels <- c(idr = "Indonesia", myr = "Malaysia", php = "Philippines",
                     sgd = "Singapore", thb = "Thailand")

country_palette <- c(idr = "#1b9e77", myr = "#d95f02", php = "#7570b3",
                      sgd = "#e7298a", thb = "#66a61e")
names(country_palette) <- country_labels[names(country_palette)]

# Non-country accents, drawn from the same Dark2 palette (positions 6-7) so
# they read as part of the same family without being mistaken for a country's
# color. Use ACCENT_COLOR for single-series charts with no country dimension
# (e.g. the pooled shock stem plot); use HIGHLIGHT_COLOR/NEUTRAL_COLOR to flag
# a subset of points (e.g. statistically significant coefficients).
ACCENT_COLOR    <- "#e6ab02"
HIGHLIGHT_COLOR <- "#a6761d"
NEUTRAL_COLOR   <- "grey55"

# Shared date-axis convention so every time-series figure ticks the same way.
scale_x_date_thesis <- function(...) {
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", ...)
}

# Shared tick-label formatters. fx_return and shock_* columns are already
# expressed in percent/bp units upstream (see 02_clean_fx.R / fig_shock_stem.R),
# so these use scale = 1 rather than re-scaling.
label_pct_thesis <- label_percent(scale = 1, accuracy = 0.1)
label_bps_thesis <- label_number(suffix = " bp", accuracy = 1)

theme_thesis <- function(base_size = FIG_BASE_SIZE, base_family = FIG_FONT) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title      = element_text(face = "bold", size = rel(1.05)),
      plot.subtitle   = element_text(colour = "grey40", size = rel(0.9)),
      plot.caption    = element_text(colour = "grey50", size = rel(0.75), hjust = 0),
      # theme_minimal() leaves panel/plot backgrounds transparent, which
      # renders unpredictably once placed on a page (some PDF viewers/print
      # pipelines composite transparency against black). Force opaque white
      # so every exported figure looks the same regardless of renderer.
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}

# Saves a vector PDF (XeLaTeX/LuaLaTeX-compatible, via the cairo_pdf device
# so the fonts above embed correctly) plus a PNG for quick preview, both at
# the same fixed dimensions so every figure in the set lines up.
save_fig <- function(p, name, width = FIG_WIDTH, height = FIG_HEIGHT) {
  ggsave(file.path(paths$out_figures, paste0(name, ".pdf")),
         p, width = width, height = height, device = cairo_pdf)
  ggsave(file.path(paths$out_figures, paste0(name, ".png")),
         p, width = width, height = height, dpi = 300)
}
