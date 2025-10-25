# R/05_report.R — HTML rapor (Özet + Grafikler + Trade istatistikleri)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(lubridate)
  library(scales); library(knitr); library(htmltools)
})

# -------------------- Klasörler --------------------
out_dir   <- "outputs"
fig_dir   <- file.path(out_dir, "figures")
tab_dir   <- file.path(out_dir, "tables")
proc_dir  <- "data/processed"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -------------------- Yol yardımcıları (rapor outputs/ içinde) --------------------
to_rel <- function(path) sub(paste0("^", out_dir, "/"), "", path)

file_html <- function(path, label = basename(path)) {
  rel <- to_rel(path)
  if (!file.exists(file.path(out_dir, rel)))
    return(HTML(sprintf("<em>Bulunamadı: %s</em>", path)))
  tags$a(href = rel, label, target = "_blank")
}

kable_html <- function(df, digits = 3) {
  if (is.null(df) || nrow(df) == 0) return(HTML("<em>Boş</em>"))
  HTML(knitr::kable(df, format = "html", digits = digits,
                    table.attr = 'class="table" style="width:auto;"'))
}

img_if <- function(path, width = "100%") {
  rel <- to_rel(path)
  if (!file.exists(file.path(out_dir, rel)))
    return(HTML(sprintf("<em>Bulunamadı: %s</em>", path)))
  tags$img(src = rel,
           style = sprintf("width:%s; max-width:1400px; display:block; margin:12px 0;", width))
}

# -------------------- Dosya yolları --------------------
sig_path      <- file.path(proc_dir, "shiller_signals.csv")
sum_path      <- file.path(tab_dir,  "summary_shiller_strategy.csv")
yr_path       <- file.path(tab_dir,  "yearly_returns_shiller_strategy.csv")
trades_path   <- file.path(tab_dir,  "trades_shiller_strategy.csv")
tr_stats_path <- file.path(tab_dir,  "trade_stats_shiller_strategy.csv")
last5_path    <- file.path(tab_dir,  "last5_trades.csv")
contrib_path  <- file.path(tab_dir,  "contributions_by_asset.csv")

fig_ind     <- file.path(fig_dir, "shiller_RSI12_SMA12.svg")
fig_regimes <- file.path(fig_dir, "ndq_regimes_raw.svg")
fig_equity  <- file.path(fig_dir, "backtest_equity_curves.svg")
fig_dd      <- file.path(fig_dir, "backtest_drawdown.svg")
fig_roll    <- file.path(fig_dir, "backtest_rolling12m.svg")

# -------------------- Sinyal / açık pozisyon --------------------
last_info <- list(txt="(bulunamadı)", date=NA)

if (file.exists(sig_path)) {
  sg <- readr::read_csv(sig_path, show_col_types = FALSE)
  
  # Signal kolonu esnek bul/üret
  cand <- intersect(c("Signal","signal","Regime","regime","State","state"), names(sg))
  if (length(cand) >= 1) {
    sg <- sg %>% mutate(Signal_std = as.integer(.data[[cand[1]]]))
  } else if (all(c("RSI12","SMA12") %in% names(sg))) {
    sg <- sg %>% mutate(Signal_std = if_else(RSI12 > SMA12, 1L, -1L))
  } else {
    sg <- tibble(Date = as.Date(NA), Signal_std = NA_integer_)
  }
  
  last <- sg %>% filter(!is.na(Signal_std)) %>% arrange(Date) %>% slice_tail(n = 1)
  if (nrow(last) == 1) {
    regime <- if (last$Signal_std[1] == 1L) "BULL (+1) → NDQ" else "BEAR (−1) → GOLD"
    last_info$txt  <- regime
    last_info$date <- last$Date[1]
  }
}

open_txt <- "—"
if (file.exists(trades_path)) {
  tr <- readr::read_csv(trades_path, show_col_types = FALSE)
  open <- tr %>% dplyr::filter(is.na(Exit_ym)) %>% dplyr::slice_tail(n = 1)
  if (nrow(open) == 1) open_txt <- sprintf("%s (giriş: %s)",
                                           open$Side[1],
                                           format(open$Entry_ym[1], "%Y-%m"))
}

# -------------------- Toplam getiriler (1995+) --------------------
tot_tbl <- NULL
if (file.exists(yr_path)) {
  yr <- readr::read_csv(yr_path, show_col_types = FALSE)
  totals <- yr %>%
    summarise(
      Strategy     = prod(1 + Strat, na.rm = TRUE) - 1,
      NDQ_BuyHold  = prod(1 + NDQ,   na.rm = TRUE) - 1,
      GOLD_BuyHold = prod(1 + GOLD,  na.rm = TRUE) - 1
    )
  
  tot_tbl <- tibble(
    Seri = c("Strategy","NDQ_BuyHold","GOLD_BuyHold"),
    `Kümülatif Getiri` = percent(c(totals$Strategy,
                                   totals$NDQ_BuyHold,
                                   totals$GOLD_BuyHold),
                                 accuracy = 0.01)
  )
}

# -------------------- Diğer tablolar --------------------
summary_tbl <- if (file.exists(sum_path)) readr::read_csv(sum_path, show_col_types=FALSE) else NULL
yr_tbl      <- if (file.exists(yr_path))  readr::read_csv(yr_path,  show_col_types=FALSE) else NULL
tr_stats    <- if (file.exists(tr_stats_path)) readr::read_csv(tr_stats_path, show_col_types=FALSE) else NULL
last5       <- if (file.exists(last5_path))    readr::read_csv(last5_path,    show_col_types=FALSE) else NULL
contrib     <- if (file.exists(contrib_path))  readr::read_csv(contrib_path,  show_col_types=FALSE) else NULL

if (!is.null(contrib)) {
  contrib <- contrib %>%
    pivot_longer(everything(), names_to="Varlık", values_to="Getiri") %>%
    mutate(Getiri = percent(Getiri, accuracy = 0.01))
}

# -------------------- Stil & başlık --------------------
css <- "
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
       line-height:1.35; margin: 24px; color:#111; }
h1 { font-size: 24px; margin: 0 0 6px 0; }
h2 { font-size: 18px; margin-top: 22px; }
h3 { font-size: 16px; }
.table { border-collapse: collapse; }
.table th, .table td { border:1px solid #ddd; padding:6px 8px; }
.small { color:#666; font-size: 12px; }
.badge { display:inline-block; padding:2px 8px; border-radius:10px; background:#eee; margin-left:6px; }
.badge.bull { background:#e6f6ea; color:#137333; }
.badge.bear { background:#fde7e9; color:#b80627; }
"

title <- "Shiller RSI(12) vs SMA(12) — Strateji Raporu"

hdr <- tags$div(
  tags$h1(title),
  tags$div(class="small",
           sprintf("Oluşturulma: %s • Veri başlangıcı: 1995-01-01",
                   format(Sys.time(), "%Y-%m-%d %H:%M %Z")))
)

curr <- tags$div(
  tags$h2("Mevcut Durum"),
  tags$p(
    sprintf("Son sinyal tarihi: %s — Rejim: ",
            ifelse(is.na(last_info$date), "—", format(last_info$date, "%Y-%m-%d"))),
    if (grepl("BULL", last_info$txt)) tags$span(class="badge bull", last_info$txt)
    else tags$span(class="badge bear", last_info$txt)
  ),
  tags$p(HTML(sprintf("Açık pozisyon: <b>%s</b>", open_txt))),
  tags$div(class="small", "Kural: BULL (+1) → %100 NDQ, BEAR (−1) → %100 GOLD (aylık Open→Open).")
)

sec_totals <- tags$div(
  tags$h2("Toplam Getiriler (1995+)"),
  if (is.null(tot_tbl)) HTML("<em>Bulunamadı: outputs/tables/yearly_returns_shiller_strategy.csv</em>")
  else kable_html(tot_tbl, digits = 2)
)

sec_summary <- tags$div(
  tags$h2("Özet Metrikler"),
  if (is.null(summary_tbl)) HTML("<em>Bulunamadı: outputs/tables/summary_shiller_strategy.csv</em>")
  else kable_html(summary_tbl, digits = 3)
)

sec_yearly <- tags$div(
  tags$h2("Yıllık Getiriler (MTM)"),
  if (is.null(yr_tbl)) HTML("<em>Bulunamadı: outputs/tables/yearly_returns_shiller_strategy.csv</em>")
  else kable_html(yr_tbl, digits = 2)
)

sec_trades <- tags$div(
  tags$h2("Trade İstatistikleri ve Kayıtlar"),
  tags$h3(style="margin-top:24px;","Özet"),
  if (is.null(tr_stats)) HTML("<em>Bulunamadı: outputs/tables/trade_stats_shiller_strategy.csv</em>")
  else kable_html(tr_stats),
  tags$h3(style="margin-top:24px;","Son 5 İşlem (PNL)"),
  if (is.null(last5)) HTML("<em>Bulunamadı: outputs/tables/last5_trades.csv</em>")
  else kable_html(last5, digits = 4),
  tags$h3(style="margin-top:24px;","Varlık Katkıları (1995+)"),
  if (is.null(contrib)) HTML("<em>Bulunamadı: outputs/tables/contributions_by_asset.csv</em>")
  else kable_html(contrib, digits = 4),
  tags$p(class="small", "Tam trade log: ", file_html(trades_path, "trades_shiller_strategy.csv"))
)

sec_figs <- tags$div(
  tags$h2("Grafikler"),
  tags$h3(style="margin-top:24px;","1) Shiller CAPE — RSI(12) & SMA(12)"), img_if(fig_ind),
  tags$h3(style="margin-top:24px;","2) Rejim Bantları Üzerinde NASDAQ (log)"), img_if(fig_regimes),
  tags$h3(style="margin-top:24px;","3) Strateji vs NDQ & GOLD — Eşitlik Eğrileri (log)"), img_if(fig_equity),
  tags$h3(style="margin-top:24px;","4) Drawdown"), img_if(fig_dd),
  tags$h3(style="margin-top:24px;","5) 12 Aylık Rolling Getiri"), img_if(fig_roll)
)

sec_files <- tags$div(
  tags$h2("Dosyalar"),
  tags$ul(
    tags$li(file_html(sum_path,     "Özet metrikler (CSV)")),
    tags$li(file_html(yr_path,      "Yıllık getiriler (CSV)")),
    tags$li(file_html(trades_path,  "Trade log (CSV)")),
    tags$li(file_html(tr_stats_path,"Trade ist. (CSV)")),
    tags$li(file_html(last5_path,   "Son 5 trade (CSV)")),
    tags$li(file_html(contrib_path, "Katkılar (CSV)")),
    tags$li(file_html(fig_ind,      "RSI-SMA (SVG)")),
    tags$li(file_html(fig_regimes,  "Rejim grafiği (SVG)")),
    tags$li(file_html(fig_equity,   "Eşitlik eğrileri (SVG)")),
    tags$li(file_html(fig_dd,       "Drawdown (SVG)")),
    tags$li(file_html(fig_roll,     "Rolling 12m (SVG)"))
  )
)

page <- tags$html(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$title(title),
    tags$style(HTML(css))
  ),
  tags$body(hdr, curr, sec_totals, sec_summary, sec_yearly, sec_trades, sec_figs, sec_files)
)

out_html <- file.path(out_dir, "report_shiller_strategy.html")
save_html(page, out_html)
cat("[Report] written ->", out_html, "\n")