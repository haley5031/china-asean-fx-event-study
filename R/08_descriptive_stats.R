# =============================================================================
# 08_descriptive_stats.R
# Generates output/tables/descriptive_stats_table.tex for the thesis.
#
# HOW TO RUN: sourced automatically by run_all.R after step 03.
# Can also be run standalone after step 03 has written its CSVs.
#
# Data source: reads data-clean/reg_data01_main.csv written by 03_build_panel.R
# (same restricted 2008-2020 sample used in all regressions, long format,
#  columns: date, country, fx_lcu_per_usd, fx_return, shock_1y, shock_5y, ...)
# =============================================================================
source("R/00_setup.R")

message("== 08: descriptive statistics ==")

# Read the estimation sample directly from the CSV step 03 wrote.
# This is safer than relying on an in-memory object name, and means
# this script runs correctly whether called from run_all.R or standalone.
reg_data <- read_csv(
  file.path(paths$clean, "reg_data01_main.csv"),
  show_col_types = FALSE
)

country_labels <- c(
  idr = "Indonesia",
  myr = "Malaysia",
  php = "Philippines",
  sgd = "Singapore",
  thb = "Thailand"
)

# --- FX return stats (one row per country) -----------------------------------
fx_stats <- reg_data %>%
  filter(!is.na(fx_return)) %>%
  group_by(country) %>%
  summarise(
    N    = n(),
    Mean = mean(fx_return),
    SD   = sd(fx_return),
    Min  = min(fx_return),
    Max  = max(fx_return),
    .groups = "drop"
  ) %>%
  mutate(label = country_labels[country]) %>%
  arrange(match(country, names(country_labels)))

# --- Shock stats (one row per DATE with nonzero shock, not per country-date) -
shock_stats <- reg_data %>%
  distinct(date, shock_1y, shock_5y) %>%
  summarise(
    across(
      c(shock_1y, shock_5y),
      list(
        N    = ~sum(.x != 0, na.rm = TRUE),
        Mean = ~mean(.x[.x != 0], na.rm = TRUE),
        SD   = ~sd(.x[.x != 0], na.rm = TRUE),
        Min  = ~min(.x[.x != 0], na.rm = TRUE),
        Max  = ~max(.x[.x != 0], na.rm = TRUE)
      )
    )
  )

# --- Format and write LaTeX table --------------------------------------------
fmt <- function(x, d = 3) formatC(as.numeric(x), digits = d, format = "f")

rows_fx <- apply(fx_stats, 1, function(r)
  sprintf("\\quad %s & %s & %s & %s & %s & %s \\\\",
          r["label"],
          formatC(as.integer(r["N"]), format = "d", big.mark = ","),
          fmt(r["Mean"]), fmt(r["SD"]), fmt(r["Min"]), fmt(r["Max"]))
)

row_1y <- sprintf(
  "\\quad One-year swap (\\texttt{shock\\_1y}) & %s & %s & %s & %s & %s \\\\",
  formatC(shock_stats$shock_1y_N, format = "d"),
  fmt(shock_stats$shock_1y_Mean), fmt(shock_stats$shock_1y_SD),
  fmt(shock_stats$shock_1y_Min),  fmt(shock_stats$shock_1y_Max)
)

row_5y <- sprintf(
  "\\quad Five-year swap (\\texttt{shock\\_5y}) & %s & %s & %s & %s & %s \\\\",
  formatC(shock_stats$shock_5y_N, format = "d"),
  fmt(shock_stats$shock_5y_Mean), fmt(shock_stats$shock_5y_SD),
  fmt(shock_stats$shock_5y_Min),  fmt(shock_stats$shock_5y_Max)
)

tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Descriptive Statistics, 2008--2020 Estimation Sample}",
  "\\label{tab:descstats}",
  "\\begin{tabular}{lccccc}",
  "\\hline",
  " & N & Mean & Std.\\ Dev. & Min & Max \\\\",
  "\\hline",
  "\\multicolumn{6}{l}{\\textit{Daily FX returns (\\%, local currency per USD)}} \\\\",
  rows_fx,
  "\\multicolumn{6}{l}{\\textit{Monetary policy surprises (basis points, nonzero event days)}} \\\\",
  row_1y,
  row_5y,
  "\\hline",
  "\\end{tabular}",
  paste0(
    "\\caption*{\\footnotesize\\textit{Note:} Daily FX returns are log differences ",
    "(Equation~\\ref{eq:fx-return}) computed for each of the five ASEAN-5 currencies ",
    "against the U.S.\\ dollar over the 2008--2020 estimation sample. ",
    "Shock statistics are computed over the ", shock_stats$shock_1y_N,
    " announcement dates carrying a nonzero one-year surprise; the five-year shock ",
    "is available on ", shock_stats$shock_5y_N, " of those same dates.}"
  ),
  "\\end{table}"
)

out_path <- file.path("output", "tables", "descriptive_stats_table.tex")
writeLines(tex, out_path)
message("Descriptive stats table written to ", out_path)
message("Upload to Overleaf as Tables/descriptive_stats_table.tex")
