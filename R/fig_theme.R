# =============================================================================
# fig_theme.R
# Shared styling for every thesis figure: font, base size, palette, and a
# save_fig() helper so all figures share dimensions and output format.
# Sourced by each fig_*.R script after R/00_setup.R.
# =============================================================================

library(ggplot2)

FIG_FONT      <- "sans"
FIG_BASE_SIZE <- 11
FIG_WIDTH     <- 7.5
FIG_HEIGHT    <- 4.5

# ASEAN-5 country labels and a consistent colorblind-friendly palette, keyed
# by the lower-case currency codes used throughout the panel.
country_labels <- c(idr = "Indonesia", myr = "Malaysia", php = "Philippines",
                     sgd = "Singapore", thb = "Thailand")

country_palette <- c(idr = "#1b9e77", myr = "#d95f02", php = "#7570b3",
                      sgd = "#e7298a", thb = "#66a61e")
names(country_palette) <- country_labels[names(country_palette)]

theme_thesis <- function(base_size = FIG_BASE_SIZE, base_family = FIG_FONT) {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.title      = element_text(face = "bold", size = rel(1.05)),
      plot.subtitle   = element_text(colour = "grey40", size = rel(0.9)),
      plot.caption    = element_text(colour = "grey50", size = rel(0.75), hjust = 0),
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
