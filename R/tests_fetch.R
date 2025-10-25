# Testler

library(readr); library(dplyr); library(lubridate)

m <- read_csv("data/processed/master_opens_monthly.csv", show_col_types = FALSE)
names(m); range(m$Date); nrow(m)
summary(m[,c("NDQ_Open","GOLD_Open","NDQ_Ret","GOLD_Ret")])
sum(is.na(m$NDQ_Ret)); sum(is.na(m$GOLD_Ret))

plot(m$Date, cumprod(1+m$NDQ_Ret), type="l", log="y", main="NDQ Open→Open (log)")

ndq <- read_csv("data/processed/ndq_monthly.csv", show_col_types = FALSE)
gold <- read_csv("data/processed/gold_monthly.csv", show_col_types = FALSE)

range(ndq$Date); range(gold$Date)
head(ndq); tail(ndq)
head(gold); tail(gold)

sh <- read_csv("data/processed/shiller_pe_clean.csv", show_col_types = FALSE)
names(sh); nrow(sh); range(sh$Date); summary(sh$Value)
head(sh, 12); tail(sh, 12)


library(dplyr); library(lubridate)

sh_m <- sh %>%
  mutate(ym = floor_date(Date, "month")) %>%
  arrange(ym) %>%
  group_by(ym) %>%
  summarise(Value = last(na.omit(Value)), .groups="drop")

nrow(sh_m); range(sh_m$ym); head(sh_m, 3); tail(sh_m, 3)
plot(sh_m$ym, sh_m$Value, type="l", main="Shiller PE (monthly)")


quantile(m$NDQ_Ret, c(0.01,0.05,0.95,0.99), na.rm=TRUE)
quantile(m$GOLD_Ret, c(0.01,0.05,0.95,0.99), na.rm=TRUE)
subset(m, NDQ_Ret > 0.4 | NDQ_Ret < -0.4) %>% select(Date, NDQ_Ret)   # ayda ±40% üstü var mı?
subset(m, GOLD_Ret > 0.4 | GOLD_Ret < -0.4) %>% select(Date, GOLD_Ret)
