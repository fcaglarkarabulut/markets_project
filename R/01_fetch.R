# R/01_fetch.R  —  Fetch & prepare data (Shiller PE from Multpl, ^NDX & XAUUSD from Stooq)

suppressPackageStartupMessages({
  library(yaml); library(httr2); library(rvest)
  library(quantmod)
  library(dplyr); library(lubridate); library(readr); library(stringr)
})

# -- Load config ---------------------------------------------------------------
cfg <- yaml::read_yaml("config.yml")

start_date <- as.Date(cfg$run$start_date)
ua         <- cfg$fetch$user_agent
save_with_date <- isTRUE(cfg$fetch$save_with_date_suffix)

sym_ndq  <- cfg$market_data$tickers$ndq
sym_gold <- cfg$market_data$tickers$gold

raw_dir  <- file.path("data","raw")
proc_dir <- file.path("data","processed")
dir.create(raw_dir,  recursive = TRUE, showWarnings = FALSE)
dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)

today_tag <- format(Sys.Date(), "%Y%m%d")
tagged_path <- function(path_no_tag) {
  if (!save_with_date) return(path_no_tag)
  ext  <- tools::file_ext(path_no_tag)
  stem <- sub(paste0("\\.", ext, "$"), "", path_no_tag)
  paste0(stem, "_", today_tag, ".", ext)
}

# -- 1) Shiller PE from Yale XLS (DATA sheet, stacked header) -----------------
library(readxl)

yale_xls_url <- cfg$sources$shiller$url
tmp_xls <- tempfile(fileext = ".xls")

cat("[Shiller/XLS] downloading:", yale_xls_url, "\n")
download.file(yale_xls_url, tmp_xls, mode = "wb", quiet = TRUE)

# Ham dosyayı audit için kaydet (tarih damgalı)
raw_xls_path <- tagged_path(file.path(raw_dir, "shiller_yale_ie_data.xls"))
file.copy(tmp_xls, raw_xls_path, overwrite = TRUE)
cat("[Shiller/XLS] raw saved ->", raw_xls_path, "\n")

# 'data' sayfasını (case-insensitive) bul
sheets <- readxl::excel_sheets(raw_xls_path)
sheet_data <- sheets[grepl("^data$", sheets, ignore.case = TRUE)]
if (length(sheet_data) == 0) sheet_data <- sheets[1]

# Başlıksız oku
tbl_raw <- readxl::read_excel(raw_xls_path, sheet = sheet_data, col_names = FALSE)
mat <- as.matrix(tbl_raw); nr <- nrow(mat); nc <- ncol(mat)

norm_txt <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))

# ------------ SHILLER (Yale XLS) — Date & CAPE'yi doğru çıkar ---------------

# yardımcılar
norm_txt <- function(x) tolower(trimws(gsub("\\s+", " ", as.character(x))))
parse_dates_year_dot_month <- function(x) {
  xn <- suppressWarnings(as.numeric(as.character(x)))
  ok <- !is.na(xn)
  yr <- floor(xn[ok])
  frac <- xn[ok] - yr
  # iki olası kodlamaya dayanıklı: "yıl.ay" (→*100) ya da yıl+kesir (→*12)
  m1 <- round(frac * 100)                      # 1871.01 → 1
  m2 <- pmax(1, pmin(12, round(frac * 12) + 1))# 1871.00→1, .083→2 …
  mm <- ifelse(m1 %in% 1:12, m1, m2)
  res <- rep(NA_Date_, length(x))
  res[ok] <- as.Date(sprintf("%04d-%02d-01", yr, mm))
  res
}

# matrisi hazırla
mat <- as.matrix(tbl_raw); nr <- nrow(mat); nc <- ncol(mat)

# 1) CAPE sütununu bul (dikey başlık: "Cyclically / Adjusted / ... / CAPE")
tokens <- norm_txt(c("Cyclically","Adjusted","Price","Earnings","Ratio","P/E10 or","CAPE"))
cap_col <- NA_integer_; cap_end_row <- NA_integer_
for (j in seq_len(nc)) {
  col_txt <- norm_txt(mat[, j, drop = TRUE])
  starts <- seq_len(max(1, nr - length(tokens) + 1))
  for (s in starts) {
    seg <- col_txt[s:(s + length(tokens) - 1)]
    if (length(seg) == length(tokens) && all(seg == tokens)) {
      cap_col <- j; cap_end_row <- s + length(tokens) - 1; break
    }
  }
  if (!is.na(cap_col)) break
}
if (is.na(cap_col)) stop("[Shiller/XLS] CAPE dikey başlığı bulunamadı.")

# 2) DATE sütununu bul: ilk sütunda "Date" yazısı var; fallback: en çok tarih parse edilen sütun
date_col <- 1
date_header_row <- which(norm_txt(mat[, date_col]) == "date")[1]
if (is.na(date_header_row)) {
  best <- list(col = NA_integer_, score = -Inf)
  for (j in seq_len(nc)) {
    d <- parse_dates_year_dot_month(mat[, j])
    sc <- sum(!is.na(d))
    if (sc > best$score) best <- list(col = j, score = sc)
  }
  date_col <- best$col
  date_header_row <- which(!is.na(parse_dates_year_dot_month(mat[, date_col])))[1] - 1
}
if (is.na(date_col) || is.na(date_header_row)) stop("[Shiller/XLS] DATE sütunu bulunamadı.")

# 3) Veri bloğunu oku
data_start <- max(cap_end_row, date_header_row) + 1
raw_dates <- mat[data_start:nr, date_col]
raw_cape  <- mat[data_start:nr, cap_col]

dates <- parse_dates_year_dot_month(raw_dates)
cape  <- suppressWarnings(readr::parse_number(as.character(raw_cape)))

sh <- tibble::tibble(Date = dates, Value = cape) |>
  dplyr::filter(!is.na(Date), !is.na(Value)) |>
  dplyr::mutate(Date = lubridate::floor_date(Date, "month")) |>
  dplyr::arrange(Date) |>
  dplyr::distinct(Date, .keep_all = TRUE)

stopifnot(nrow(sh) > 1500)  # 1870'lerden bugüne ~1800+ aylık satır bekleriz

proc_csv_sh <- file.path(proc_dir, "shiller_pe_clean.csv")
readr::write_csv(sh, proc_csv_sh, na = "")
cat("[Shiller/XLS] cleaned CSV saved ->", proc_csv_sh,
    " first:", format(min(sh$Date), "%Y-%m-%d"),
    " last:",  format(max(sh$Date), "%Y-%m-%d"),
    " rows:",  nrow(sh), "\n")
# ---------------------------------------------------------------------------
# -- 2) Fetch from Stooq CSV (monthly) ---------------------------------------
fetch_stooq_csv_monthly <- function(symbol, name_slug) {
  base <- "https://stooq.com/q/d/l/"
  sym_param <- tolower(symbol)  # stooq genelde küçük harf kabul ediyor
  url <- paste0(base, "?s=", utils::URLencode(sym_param, reserved = TRUE), "&i=m")
  cat("[Stooq/CSV] downloading:", url, "\n")
  
  resp <- httr2::request(url) |>
    httr2::req_user_agent(ua) |>
    httr2::req_perform()
  
  # ham CSV'yi kaydet (audit)
  raw_csv <- tagged_path(file.path(raw_dir, paste0(name_slug, "_stooq_monthly.csv")))
  writeBin(httr2::resp_body_raw(resp), raw_csv)
  cat("[Stooq/CSV] raw monthly saved ->", raw_csv, "\n")
  
  df <- readr::read_csv(raw_csv, show_col_types = FALSE, na = c("", "null", "NA")) |>
    dplyr::mutate(Date = as.Date(Date)) |>
    dplyr::arrange(Date)
  
  # beklenen kolonlar kontrolü
  stopifnot(all(c("Date","Open","High","Low","Close") %in% names(df)))
  
  proc_csv <- file.path(proc_dir, paste0(name_slug, "_monthly.csv"))
  readr::write_csv(df, proc_csv)
  cat("[Stooq/CSV] monthly saved ->", proc_csv,
      " first:", format(min(df$Date), "%Y-%m-%d"),
      " last:",  format(max(df$Date), "%Y-%m-%d"),
      " rows:",  nrow(df), "\n")
  
  df
}

ndq_m  <- fetch_stooq_csv_monthly(sym_ndq,  "ndq")
gold_m <- fetch_stooq_csv_monthly(sym_gold, "gold")

# -- 3) Master monthly opens + Open→Open returns ------------------------------
master <- ndq_m |>
  select(Date, NDQ_Open = Open) |>
  left_join(gold_m |> select(Date, GOLD_Open = Open), by = "Date") |>
  arrange(Date) |>
  mutate(
    NDQ_Ret  = NDQ_Open  / lag(NDQ_Open)  - 1,
    GOLD_Ret = GOLD_Open / lag(GOLD_Open) - 1
  ) |>
  filter(Date >= start_date)

proc_master <- file.path(proc_dir, "master_opens_monthly.csv")
write_csv(master, proc_master)
cat("[Master] saved ->", proc_master, " rows:", nrow(master), "\n")

