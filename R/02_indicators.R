# R/02_indicators.R  — Shiller CAPE RSI(12) + SMA(12) (grafik 1990+), tek SVG

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(lubridate)
  library(TTR);   library(zoo);   library(ggplot2); library(svglite)
})

# 1) Veriyi oku (tam tarih aralığı: 1881+)
sh <- read_csv("data/processed/shiller_pe_clean.csv", show_col_types = FALSE) %>%
  arrange(Date)

stopifnot(all(c("Date","Value") %in% names(sh)))

# 2) RSI(12) ve RSI'nin SMA(12)'si — HESAP HER ZAMAN TAM SERİ ÜZERİNDEN
sh_ind <- sh %>%
  mutate(
    rsi12 = as.numeric(RSI(Value, n = 12)),                       # 0–100
    sma12 = as.numeric(rollmean(rsi12, k = 12, align = "right", fill = NA))
  )

# İndikatörleri (tam tarihli) CSV’ye yaz (opsiyonel ama faydalı)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(sh_ind, "data/processed/shiller_indicators.csv")

# 3) Grafiği 1990'dan itibaren çiz
p_start <- as.Date("1990-01-01")
plt_df <- sh_ind %>% filter(Date >= p_start)

# 4) GGPlot: RSI kırmızı, SMA mavi; ikisi de kesiksiz
p <- ggplot(plt_df, aes(x = Date)) +
  geom_line(aes(y = rsi12, colour = "RSI(12)"), linewidth = 0.7) +
  geom_line(aes(y = sma12, colour = "SMA(12)"), linewidth = 0.7) +
  scale_colour_manual(values = c("RSI(12)" = "#d62728",   # kırmızı
                                 "SMA(12)" = "#1f77b4"))  # mavi
# İstersen referans çizgileri aç:
# + geom_hline(yintercept = c(30, 70), linetype = "dashed", linewidth = 0.4)

p <- p +
  labs(title = "Shiller CAPE — RSI(12) ve SMA(12) (grafik 1990+)",
       subtitle = "Hesap 1881+ tüm seri üzerinde; gösterim 1990'dan itibaren",
       x = NULL, y = "RSI (0–100)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

# 5) Tek çıktı: SVG
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)
svg_path <- "outputs/figures/shiller_RSI12_SMA12_1990.svg"
ggsave(svg_path, plot = p, width = 1600, height = 700, units = "px", dpi = 144)
cat("[Figure] SVG saved ->", svg_path, "\n")