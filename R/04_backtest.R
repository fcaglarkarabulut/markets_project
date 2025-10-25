# R/04_backtest.R — Shiller RSI(12) vs SMA(12) sinyaliyle NDQ / GOLD O→O backtest
# Kural: Signal=+1 → %100 NDQ, Signal=-1 → %100 GOLD
# Uygulama: sinyal apply_lag_months kadar gecikmeli (O→O için 1 mantıklı)

suppressPackageStartupMessages({
  library(magrittr)   # %>%
  library(readr); library(dplyr); library(tidyr); library(lubridate)
  library(ggplot2); library(scales); library(slider); library(yaml); library(purrr)
})

# ------------------------------ Ayarlar / Klasörler ------------------------------
proc_dir <- "data/processed"
fig_dir  <- "outputs/figures"
tab_dir  <- "outputs/tables"
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

start_date <- as.Date("1995-01-01")
if (file.exists("config.yml")) {
  cfg <- tryCatch(yaml::read_yaml("config.yml"), error = function(e) NULL)
  if (!is.null(cfg$run$start_date)) start_date <- as.Date(cfg$run$start_date)
}

risk_free_monthly <- 0
apply_lag_months  <- 1   # 0 yaparsan gecikme yok

# ------------------------------ Yardımcılar -------------------------------------
to_equity <- function(r) cumprod(1 + r)

ann_sd <- function(x) sd(x, na.rm = TRUE) * sqrt(12)

dd_vec <- function(eq) {
  peak <- cummax(eq)
  draw <- eq/peak - 1
  list(series = draw, mdd = suppressWarnings(min(draw, na.rm = TRUE)))
}

metric_block <- function(ret_vec, eq_vec) {
  n <- length(ret_vec)
  yrs <- n/12
  cagr   <- (tail(eq_vec, 1))^(1/yrs) - 1
  stdev  <- ann_sd(ret_vec)
  sharpe <- if (isTRUE(all.equal(stdev, 0))) NA_real_ else ((mean(ret_vec, na.rm = TRUE) - risk_free_monthly) * 12) / stdev
  mdd    <- dd_vec(eq_vec)$mdd
  tibble(CAGR=cagr, Stdev_Ann=stdev, Sharpe=sharpe, MaxDD=mdd)
}

# Sinyali güvenli yükle: shiller_signals.csv yoksa/uygun değilse indicators'tan üret
load_signals <- function(proc_dir) {
  s_path <- file.path(proc_dir, "shiller_signals.csv")
  if (file.exists(s_path)) {
    sg <- readr::read_csv(s_path, show_col_types = FALSE)
    nm <- tolower(names(sg)); names(sg) <- nm
    # tarih kolonu adayları
    date_col <- NULL
    for (cand in c("date","ym","month")) if (cand %in% names(sg)) { date_col <- cand; break }
    if (is.null(date_col)) stop("[Backtest] shiller_signals.csv içinde tarih kolonu yok.")
    # sinyal kolonu adayları
    sig_col <- NULL
    for (cand in c("signal","regime","bullbear")) if (cand %in% names(sg)) { sig_col <- cand; break }
    if (!is.null(sig_col)) {
      out <- sg %>%
        mutate(ym = lubridate::floor_date(as.Date(.data[[date_col]]), "month")) %>%
        transmute(
          ym,
          Signal = dplyr::case_when(
            is.numeric(.data[[sig_col]]) ~ ifelse(.data[[sig_col]] >= 0, 1L, -1L),
            tolower(as.character(.data[[sig_col]])) %in% c("1","+1","bull","bull (+1)","bullish") ~ 1L,
            TRUE ~ -1L
          )
        ) %>%
        arrange(ym) %>% distinct(ym, .keep_all = TRUE)
      return(out)
    }
    # sinyal kolonu yoksa indicators'a düş
  }
  # Fallback: göstergeden sinyal üret (RSI12 > SMA12 → +1, aksi halde -1)
  i_path <- file.path(proc_dir, "shiller_indicators.csv")
  if (!file.exists(i_path)) stop("[Backtest] shiller_indicators.csv bulunamadı.")
  ind <- readr::read_csv(i_path, show_col_types = FALSE)
  nm  <- tolower(names(ind)); names(ind) <- nm
  if (!all(c("date","rsi12","sma12") %in% names(ind))) {
    stop("[Backtest] shiller_indicators.csv içinde beklenen sütunlar yok (date, rsi12, sma12).")
  }
  message("[Backtest] 'Signal' bulunamadı; RSI(12)>SMA(12) ile sinyal yeniden üretildi.")
  ind %>%
    mutate(ym = lubridate::floor_date(as.Date(date), "month")) %>%
    transmute(ym, Signal = ifelse(rsi12 > sma12, 1L, -1L)) %>%
    arrange(ym) %>% distinct(ym, .keep_all = TRUE)
}

# ------------------------------ Veriler ---------------------------------------
m <- readr::read_csv(file.path(proc_dir, "master_opens_monthly.csv"), show_col_types = FALSE) %>%
  mutate(ym = lubridate::floor_date(Date, "month")) %>%
  filter(ym >= start_date) %>%
  arrange(ym)

sg <- load_signals(proc_dir)

bt <- m %>%
  left_join(sg, by = "ym") %>%
  arrange(ym) %>%
  mutate(
    Signal_applied = dplyr::lag(Signal, n = apply_lag_months),
    Strat_Ret = dplyr::case_when(
      Signal_applied ==  1 ~ NDQ_Ret,
      Signal_applied == -1 ~ GOLD_Ret,
      TRUE ~ 0
    ),
    NDQ_BH  = NDQ_Ret,
    GOLD_BH = GOLD_Ret
  ) %>%
  filter(!is.na(Signal_applied))

if (nrow(bt) < 12) stop("[Backtest] Yeterli gözlem yok (lag sonrası).")

bt <- bt %>%
  mutate(
    Eq_Strategy = to_equity(Strat_Ret),
    Eq_NDQ_BH   = to_equity(NDQ_BH),
    Eq_GOLD_BH  = to_equity(GOLD_BH)
  )

# ------------------------------ Metrikler --------------------------------------
sum_strategy <- metric_block(bt$Strat_Ret, bt$Eq_Strategy)
sum_ndq      <- metric_block(bt$NDQ_BH,    bt$Eq_NDQ_BH)
sum_gold     <- metric_block(bt$GOLD_BH,   bt$Eq_GOLD_BH)

summary_tbl <- tibble(
  Metric = c("CAGR","Stdev_Ann","Sharpe","MaxDrawdown"),
  Strategy    = c(sum_strategy$CAGR, sum_strategy$Stdev_Ann, sum_strategy$Sharpe, sum_strategy$MaxDD),
  NDQ_BuyHold = c(sum_ndq$CAGR,      sum_ndq$Stdev_Ann,      sum_ndq$Sharpe,      sum_ndq$MaxDD),
  GOLD_BuyHold= c(sum_gold$CAGR,     sum_gold$Stdev_Ann,     sum_gold$Sharpe,     sum_gold$MaxDD)
)
readr::write_csv(summary_tbl, file.path(tab_dir, "summary_shiller_strategy.csv"))

# --------------------------- Yıllık getiriler ----------------------------------
yearly_tbl <- bt %>%
  mutate(Year = lubridate::year(ym)) %>%
  group_by(Year) %>%
  summarise(
    Strat = prod(1 + Strat_Ret, na.rm = TRUE) - 1,
    NDQ   = prod(1 + NDQ_BH,    na.rm = TRUE) - 1,
    GOLD  = prod(1 + GOLD_BH,   na.rm = TRUE) - 1,
    .groups = "drop"
  )
readr::write_csv(yearly_tbl, file.path(tab_dir, "yearly_returns_shiller_strategy.csv"))

# ------------------------------ Trade log --------------------------------------
# Pos değişimi: ilk satırı TRUE işaretle, sonra fark al
bt$Pos <- bt$Signal_applied
bt$Pos_Change <- c(TRUE, diff(bt$Pos) != 0)

starts <- which(bt$Pos_Change)
ends   <- c(starts[-1] - 1, nrow(bt))

trade_ret <- purrr::map2_dbl(starts, ends, function(s, e){
  if (bt$Pos[s] == 1) prod(1 + bt$NDQ_Ret[s:e],  na.rm = TRUE) - 1
  else                prod(1 + bt$GOLD_Ret[s:e], na.rm = TRUE) - 1
})

trades <- tibble(
  Entry_ym = bt$ym[starts],
  Exit_ym  = bt$ym[ends],
  Side     = ifelse(bt$Pos[starts] == 1, "NDQ", "GOLD"),
  Return   = trade_ret
)
readr::write_csv(trades, file.path(tab_dir, "trades_shiller_strategy.csv"))
readr::write_csv(utils::tail(trades, 5), file.path(tab_dir, "last5_trades.csv"))

ndq_contrib  <- prod(1 + bt$NDQ_Ret [bt$Pos ==  1], na.rm = TRUE) - 1
gold_contrib <- prod(1 + bt$GOLD_Ret[bt$Pos == -1], na.rm = TRUE) - 1
readr::write_csv(tibble(NDQ = ndq_contrib, GOLD = gold_contrib),
                 file.path(tab_dir, "contributions_by_asset.csv"))

win_rate <- mean(trades$Return > 0)
avg_win  <- mean(trades$Return[trades$Return > 0],  na.rm = TRUE)
avg_loss <- mean(trades$Return[trades$Return <= 0], na.rm = TRUE)
readr::write_csv(tibble(Trades=nrow(trades), WinRate=win_rate, AvgWin=avg_win, AvgLoss=avg_loss),
                 file.path(tab_dir, "trade_stats_shiller_strategy.csv"))

# ------------------------------- Grafikler -------------------------------------
eq_df <- bt %>%
  select(ym, Strategy = Eq_Strategy, NDQ_BuyHold = Eq_NDQ_BH, GOLD_BuyHold = Eq_GOLD_BH) %>%
  pivot_longer(-ym, names_to = "Series", values_to = "Eq")

p1 <- ggplot(eq_df, aes(ym, Eq, colour = Series)) +
  geom_line(linewidth = 0.7) +
  scale_y_log10(labels = scales::label_number()) +
  scale_colour_manual(values = c("Strategy"="#0B7285","NDQ_BuyHold"="#444444","GOLD_BuyHold"="#8D6E63")) +
  labs(title="Strateji vs NDQ & GOLD — Eşitlik Eğrileri (log)", x=NULL, y="Equity (log)") +
  theme_minimal(base_size=12) + theme(legend.position="bottom")
ggsave(file.path(fig_dir, "backtest_equity_curves.svg"), p1, width = 12, height = 6)

dd_series <- dd_vec(bt$Eq_Strategy)$series
p2 <- ggplot(data.frame(ym=bt$ym, DD=dd_series), aes(ym, DD)) +
  geom_area(fill="#ef476f", alpha=0.35) +
  scale_y_continuous(labels = percent) +
  labs(title="Strateji Drawdown", x=NULL, y="DD") +
  theme_minimal(base_size=12)
ggsave(file.path(fig_dir, "backtest_drawdown.svg"), p2, width = 12, height = 4)

roll12 <- slider::slide_dbl(bt$Strat_Ret, ~ prod(1 + .x) - 1, .before = 11, .complete = TRUE)
p3 <- ggplot(data.frame(ym=bt$ym, R12=roll12), aes(ym, R12)) +
  geom_line(color="#0B7285") + geom_hline(yintercept = 0, linetype = 3) +
  scale_y_continuous(labels = percent) +
  labs(title="Strateji 12 Aylık Rolling Getiri", x=NULL, y="12m Return") +
  theme_minimal(base_size=12)
ggsave(file.path(fig_dir, "backtest_rolling12m.svg"), p3, width = 12, height = 4)

# ------------------------------- Konsol özeti ----------------------------------
last_pos <- utils::tail(bt$Pos, 1)
last_txt <- ifelse(last_pos==1, "NDQ (Long)", "GOLD (Long)")
cat(sprintf("[Backtest] Last position: %s as of %s\n", last_txt, format(utils::tail(bt$ym,1), "%Y-%m-%d")))
cat("[Backtest] Summary saved ->", file.path(tab_dir, "summary_shiller_strategy.csv"), "\n")
cat("[Backtest] Yearly returns ->", file.path(tab_dir, "yearly_returns_shiller_strategy.csv"), "\n")
cat("[Backtest] Trades log ->", file.path(tab_dir, "trades_shiller_strategy.csv"), "\n")
cat("[Backtest] Equity curves ->", file.path(fig_dir, "backtest_equity_curves.svg"), "\n")