
rm(list = ls())
graphics.off()

setwd("C:/Users/justi/Documents/Y2S2/BT4014/project")

library(tsm)
library(vars)
library(mFilter)
library(anytime)
library(readr)
library(tseries)


mondat <- read.csv("C:/Users/justi/Documents/Y2S2/BT4014/project/monero_biweekly.csv")
drugdat <- read.csv("C:/Users/justi/Documents/Y2S2/BT4014/project/ecstasy_biweekly.csv")
bitdat <- read.csv("C:/Users/justi/Documents/Y2S2/BT4014/project/bitcoin_biweekly.csv")

#timeseries
monprice <- ts(mondat$Price)
bitprice <- ts(bitdat$Closing.Price..USD.)
drugsales <- ts(drugdat$sales)

#difference
monprice.diff = diff(monprice, differences = 1)
bitprice.diff = diff(bitprice, differences = 1)
drugsales.diff = diff(drugsales, differences = 1)

#VAR model
mv.est <- VAR(dat.mv, p = 1, type = "const", season = NULL, 
              exog = NULL)
model_summary <- summary(mv.est)


bv12.est <- VAR(dat.bv12, p = 1, type = "const", season = NULL, 
              exog = NULL)
bv23.est <- VAR(dat.bv23, p = 1, type = "const", season = NULL, 
              exog = NULL)

#granger causality and contemporaneous rship (2 way)

bv12.cause.d1p1 <- causality(bv12.est, cause = "Monero_Price")
bv12.cause.d1p1
bv12.cause.d1p2 <- causality(bv12.est, cause = "Drug_Sales")
bv12.cause.d1p2
bv23.cause.d1p2 <- causality(bv23.est, cause = "Drug_Sales")
bv23.cause.d1p2
bv23.cause.d1p3 <- causality(bv23.est, cause = "Bitcoin_Price")
bv23.cause.d1p3

