# R/03_signals.R — Shiller RSI(12) vs SMA(12) sinyali üretir ve
# NASDAQ (log) üzerinde rejim bantlarını çizer.

suppressPackageStartupMessages({
  library(yaml)
  library(readr); library(dplyr); library(lubridate)
  library(ggplot2); library(scales)
  library(TTR)
})

# --- Config & yollar ----------------------------------------------------------
cfg <- yaml::read_yaml("config.yml")

proc_dir <- file.path("data","processed")
fig_dir  <- file.path("outputs","figures")
dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(fig_dir,  recursive = TRUE, showWarnings = FALSE)

start_show <- as.Date(if (!is.null(cfg$run$start_date)) cfg$run$start_date else "1995-01-01")

# fig boyutları (px -> inch)
out <- cfg$output
w_px <- if (!is.null(out$fig_width))  out$fig_width  else 1600
h_px <- if (!is.null(out$fig_height)) out$fig_height else 800
dpi  <- 96
w_in <- w_px / dpi
h_in <- h_px / dpi

# --- Verileri oku -------------------------------------------------------------
# Shiller CAPE (temiz)
sh <- readr::read_csv(file.path(proc_dir, "shiller_pe_clean.csv"), show_col_types = FALSE) |>
  arrange(Date)

# NASDAQ (aylık)
ndq <- readr::read_csv(file.path(proc_dir, "ndq_monthly.csv"), show_col_types = FALSE) |>
  transmute(Date = as.Date(Date), Close = as.numeric(Close)) |>
  arrange(Date)

# --- RSI(12) ve SMA(12) + rejim ----------------------------------------------
rsi_n <- 12L
ma_n  <- 12L

sh <- sh |>
  mutate(
    RSI12 = TTR::RSI(Value, n = rsi_n),
    SMA12 = TTR::SMA(RSI12, n = ma_n),
    Regime = ifelse(RSI12 > SMA12, 1L, -1L)
  )

# Sinyali kaydet
sig_path <- file.path(proc_dir, "shiller_signals.csv")
readr::write_csv(sh, sig_path)
last_row <- tail(sh[complete.cases(sh$RSI12, sh$SMA12), ], 1)

cat("[Signals] saved ->", sig_path, " rows:", nrow(sh), "\n")
cat(
  sprintf("[Signals] Last regime: %s (%+d) on %s | RSI12=%.2f SMA12=%.2f CAPE=%.2f\n",
          ifelse(last_row$Regime == 1, "BULL", "BEAR"),
          last_row$Regime,
          format(last_row$Date, "%Y-%m-%d"),
          last_row$RSI12, last_row$SMA12, last_row$Value)
)

# --- Rejim bandı grafiği (NDQ log) -------------------------------------------
# Gösterim aralığı
ndq_plot <- ndq |> filter(Date >= start_show)
regimes  <- sh  |> filter(Date >= start_show) |>
  select(Date, Regime)

# Bantlar (log eksende NaN uyarısı vermemesi için pozitif y aralığı seç)
y_min <- min(ndq_plot$Close, na.rm = TRUE) * 0.95
y_max <- max(ndq_plot$Close, na.rm = TRUE) * 1.05

reg_plot <- regimes |>
  mutate(
    xmin = Date,
    xmax = lead(Date, default = (max(ndq_plot$Date, na.rm = TRUE) + months(1))),
    ymin = y_min,
    ymax = y_max,
    regime_lab = ifelse(Regime == 1, "Bull (+1)", "Bear (-1)")
  )

p <- ggplot() +
  # Arka plan rejim bantları
  geom_rect(
    data = reg_plot,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = regime_lab),
    inherit.aes = FALSE, alpha = 0.20
  ) +
  scale_fill_manual(
    values = c("Bear (-1)" = "#ff6b6b", "Bull (+1)" = "#6abf69"),
    name = NULL
  ) +
  # NDQ log çizgisi
  geom_line(data = ndq_plot, aes(Date, Close), linewidth = 0.6, colour = "black") +
  scale_y_log10(
    labels = scales::label_number(scale_cut = scales::cut_si("")),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    title    = "Shiller RSI(12) vs SMA(12) Rejimleri",
    subtitle = "Arka plan: rejim • Önde: NASDAQ (Close, log ölçek)",
    x = NULL, y = "NASDAQ (log)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

fig_path <- file.path(fig_dir, "ndq_regimes_raw.svg")
ggsave(fig_path, plot = p, width = w_in, height = h_in, dpi = dpi)
cat("[Figure] saved ->", fig_path, "\n")