# =============================================================================
# 14_dollar_control_fix.R
# Re-runs the control ladder of R/13 check 2 with a dollar index that does not
# contain either side of the regression.
#
# THE PROBLEM. DTWEXBGS is the *broad* nominal dollar index. Its largest single
# component is the renminbi, and the Indonesian rupiah, Malaysian ringgit, Thai
# baht, Singapore dollar and Philippine peso are all constituents as well. Using
# it as a control therefore does two things at once, both of them wrong:
#
#   (i)  it partially controls for the renminbi, which -- if ASEAN currencies
#        respond to PBoC surprises by co-moving with the RMB -- is the
#        transmission channel itself, not a confounder. Controlling for a
#        mediator absorbs the effect being estimated.
#   (ii) it contains the dependent variable. The ASEAN-5 currencies enter the
#        index directly, so the regressor is mechanically correlated with the
#        outcome regardless of any policy shock.
#
# The halving of the interaction term in R/13 check 2 is therefore not evidence
# against the post-2015 result. It is what a bad control does.
#
# THE FIX. DTWEXAFEGS is the nominal dollar index against ADVANCED foreign
# economies: the euro area, Japan, the United Kingdom, Canada, Switzerland,
# Australia and Sweden. It excludes China and excludes every ASEAN-5 currency,
# so it measures general dollar strength without containing either the channel
# or the outcome. This is the control the specification wants.
#
# Both are reported side by side, because the contrast is itself the argument.
#
# Input : data-clean/reg_data_ext_main.csv
# Output: output/tables/regime_dollar_control.csv
# =============================================================================

source("R/00_setup.R")

panel <- read_csv(file.path(paths$clean, "reg_data_ext_main.csv"),
                  show_col_types = FALSE) %>%
  arrange(country, date)

SHOCK <- "shock_1y"

# --- Fetch the advanced-economies dollar index -------------------------------
afe_id   <- "DTWEXAFEGS"
afe_file <- file.path(paths$raw_external, paste0(afe_id, ".csv"))

if (!file.exists(afe_file)) {
  message("Downloading ", afe_id, " from FRED ...")
  url <- sprintf(
    "https://fred.stlouisfed.org/graph/fredgraph.csv?id=%s&cosd=2005-01-01&coed=2020-12-31",
    afe_id)
  ok <- tryCatch({ download.file(url, afe_file, quiet = TRUE, mode = "wb"); TRUE },
                 error = function(e) { message("  FAILED: ", conditionMessage(e)); FALSE })
  if (!ok || !file.exists(afe_file) || file.size(afe_file) < 50)
    stop("Could not download ", afe_id, ". Fetch manually from:\n  ", url,
         "\nand save as ", afe_file)
} else {
  message("Using cached ", basename(afe_file))
}

afe_raw <- read_csv(afe_file, col_types = cols(.default = col_character()))
afe <- data.frame(date = as.Date(afe_raw[[1]]),
                  dollar_afe = suppressWarnings(as.numeric(afe_raw[[2]])))
afe <- afe[!is.na(afe$date), ]

# --- Build the control on the FX trading calendar ----------------------------
# Same discipline as R/10: lag() must step to the previous ASEAN trading day,
# not the previous FRED day, or the control is measured over a different
# interval from the outcome.
fx_dates <- panel %>% distinct(date) %>% arrange(date)

afe_ctrl <- fx_dates %>%
  left_join(afe, by = "date") %>%
  arrange(date) %>%
  mutate(ret_dollar_afe = 100 * (log(dollar_afe) - log(lag(dollar_afe)))) %>%
  mutate(ctrl_afe_w01 = ret_dollar_afe + lead(ret_dollar_afe, 1)) %>%
  select(date, ctrl_afe_w01)

panel <- panel %>% left_join(afe_ctrl, by = "date")

# --- Common sample across BOTH dollar measures -------------------------------
# Nested columns require one fixed sample. Requiring both indices to be present
# means the broad-index and AFE-index columns are directly comparable.
common <- panel %>%
  filter(!is.na(usd_w01), !is.na(ctrl_dollar_w01), !is.na(ctrl_afe_w01),
         !is.na(ctrl_ust2y_w01), !is.na(ctrl_vix_w01), !is.na(ctrl_brent_w01))

message("Common-sample rows: ", nrow(common))

# How correlated are the two dollar measures with each other, and each with the
# renminbi leg? If the broad index is largely the RMB, that shows up here.
cors <- common %>%
  mutate(rmb_leg = usd_w01 - cny_w01) %>%
  filter(!is.na(rmb_leg)) %>%
  summarise(
    `broad vs AFE`      = cor(ctrl_dollar_w01, ctrl_afe_w01),
    `broad vs RMB leg`  = cor(ctrl_dollar_w01, rmb_leg),
    `AFE vs RMB leg`    = cor(ctrl_afe_w01,    rmb_leg)
  )
cat("\n=== Correlations among the dollar measures and the renminbi leg ===\n")
print(as.data.frame(cors), row.names = FALSE, digits = 3)
cat("\nIf 'broad vs RMB leg' is materially higher than 'AFE vs RMB leg', the\n",
    "broad index is absorbing the renminbi channel and should not be used as a\n",
    "control in this specification.\n", sep = "")

# --- The ladder --------------------------------------------------------------
f <- function(rhs) as.formula(sprintf(
  "usd_w01 ~ %s * post_split%s | country",
  SHOCK, if (length(rhs)) paste0(" + ", paste(rhs, collapse = " + ")) else ""))

m0 <- feols(f(NULL),                cluster = ~date, data = common)
mB <- feols(f("ctrl_dollar_w01"),   cluster = ~date, data = common)
mA <- feols(f("ctrl_afe_w01"),      cluster = ~date, data = common)
mA2 <- feols(f(c("ctrl_afe_w01", "ctrl_ust2y_w01",
                 "ctrl_vix_w01", "ctrl_brent_w01")),
             cluster = ~date, data = common)

cat("\n\n=== Post-2015 interaction under alternative dollar controls ===\n")
print(etable(
  list("(1) none"            = m0,
       "(2) broad USD [bad]" = mB,
       "(3) AFE USD"         = mA,
       "(4) AFE + all"       = mA2),
  cluster = ~date, digits = 3, fitstat = c("n", "r2", "wr2")))

cat("\nColumn (2) is reported to be shown, not relied on: the broad index\n",
    "contains the renminbi and all five ASEAN currencies. Column (3) is the\n",
    "specification the design calls for. If the interaction survives (3) and\n",
    "(4), the post-2015 response is not general dollar movement.\n", sep = "")

# --- Save --------------------------------------------------------------------
out <- bind_rows(
  coef_row(m0,  paste0(SHOCK, ":post_split"), "no controls"),
  coef_row(mB,  paste0(SHOCK, ":post_split"), "broad dollar (contains RMB + ASEAN)"),
  coef_row(mA,  paste0(SHOCK, ":post_split"), "AFE dollar (excludes RMB + ASEAN)"),
  coef_row(mA2, paste0(SHOCK, ":post_split"), "AFE dollar + UST2Y + VIX + oil")
)
cat("\n\n=== Interaction term summary ===\n\n")
print(out, row.names = FALSE, digits = 3)
write_csv(out, file.path(paths$out_tables, "regime_dollar_control.csv"))

message("\nSaved: output/tables/regime_dollar_control.csv")
