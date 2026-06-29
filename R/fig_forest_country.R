# =============================================================================
# fig_forest_country.R
# Figure 3a: forest plot of the five country-level betas (Table 1), with 95%
# CIs, so that the one significant coefficient is visually obvious against
# the rest.
#
# Input : output/models.rds  (models$country_ols)
# Output: output/figures/fig3a_forest_country.{pdf,png}
# =============================================================================

source("R/00_setup.R")
source("R/fig_theme.R")

models <- readRDS(file.path(paths$output_root, "models.rds"))

coef_df <- lapply(names(models$country_ols), function(cc) {
  m  <- models$country_ols[[cc]]
  ci <- confint(m)["shock_1y", ]
  data.frame(
    country  = cc,
    estimate = coef(m)["shock_1y"],
    lower    = ci[1],
    upper    = ci[2]
  )
}) %>% bind_rows() %>%
  mutate(
    country     = country_labels[country],
    significant = lower > 0 | upper < 0
  )

coef_df <- coef_df[order(coef_df$estimate), ]
coef_df$country <- factor(coef_df$country, levels = coef_df$country)

p <- ggplot(coef_df, aes(x = estimate, y = country)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbar(aes(xmin = lower, xmax = upper, colour = significant),
                width = 0.15, linewidth = 0.6) +
  geom_point(aes(colour = significant), size = 2.5) +
  scale_colour_manual(values = c(`TRUE` = "#d95f02", `FALSE` = "grey55"),
                       guide = "none") +
  labs(
    title    = "Country-Level Exchange Rate Responses to Chinese MP Shocks",
    subtitle = "Coefficient on shock_1y, 95% CIs, [0,+1] window; orange = CI excludes zero",
    x        = "Coefficient on shock_1y",
    y        = NULL
  ) +
  theme_thesis()

save_fig(p, "fig3a_forest_country")
message("Saved fig3a_forest_country to ", paths$out_figures)
