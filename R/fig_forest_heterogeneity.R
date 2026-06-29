# =============================================================================
# fig_forest_heterogeneity.R
# Figure 3b: forest plot of the Table 3 heterogeneity interactions --
# Indonesia's baseline shock_1y coefficient plus the four country-relative
# interaction terms -- with 95% CIs, to make the one significant difference
# visually obvious against the rest.
#
# Input : output/models.rds  (models$fe_het_1y)
# Output: output/figures/fig3b_forest_heterogeneity.{pdf,png}
# =============================================================================

source("R/00_setup.R")
source("R/fig_theme.R")

models <- readRDS(file.path(paths$output_root, "models.rds"))
m      <- models$fe_het_1y

term_map <- c(
  "shock_1y"              = "Indonesia (baseline)",
  "shock_1y:country::myr"  = "Malaysia (rel. to Indonesia)",
  "shock_1y:country::php"  = "Philippines (rel. to Indonesia)",
  "shock_1y:country::sgd"  = "Singapore (rel. to Indonesia)",
  "shock_1y:country::thb"  = "Thailand (rel. to Indonesia)"
)

ci <- confint(m)
het_df <- data.frame(
  term     = names(term_map),
  label    = term_map[names(term_map)],
  estimate = coef(m)[names(term_map)],
  lower    = ci[names(term_map), 1],
  upper    = ci[names(term_map), 2]
) %>%
  mutate(significant = lower > 0 | upper < 0)

het_df <- het_df[order(het_df$estimate), ]
het_df$label <- factor(het_df$label, levels = het_df$label)

p <- ggplot(het_df, aes(x = estimate, y = label)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lower, xmax = upper, colour = significant),
                width = 0.15, linewidth = 0.6) +
  geom_point(aes(colour = significant), size = 2.5) +
  scale_colour_manual(values = c(`TRUE` = "#d95f02", `FALSE` = "grey55"),
                       guide = "none") +
  labs(
    title    = "Heterogeneity in FX Response to Chinese MP Shocks",
    subtitle = "Indonesia baseline and country-relative interaction terms, 95% CIs; orange = CI excludes zero",
    x        = "Coefficient (shock_1y)",
    y        = NULL
  ) +
  theme_thesis()

save_fig(p, "fig3b_forest_heterogeneity")
message("Saved fig3b_forest_heterogeneity to ", paths$out_figures)
