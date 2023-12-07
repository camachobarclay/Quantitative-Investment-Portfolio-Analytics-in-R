library(quantmod)
library(FactorAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(PortfolioAnalytics)
library(quadprog)
library(ROI.plugin.quadprog)
library(TTR)
library(xts)
library(fPortfolio)
library(fAssets)
library(DEoptimR)
library(timeSeries)
library(MASS)
library(mvtnorm)
library(QRM)
library(boot)
library(fExtremes)
library(vars)
library(ggplot2)
library(scales)

symbols <-c("SPY",
            "AGG",
            "VEA")

getSymbols(symbols, src = "yahoo")
prices <- do.call(merge,
                  lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <-symbols

getSymbols.alphavantage("symbols", api.key = "QO81L3TCYP5JOA9E")

fred.tickers <-c("RU1000TR", "DGS10")
getSymbols(fred.tickers,src = "FRED")

library(Quandl)

oil.1<-Quandl("NSE/OIL",
              start_date = "2013-01-01",
              collapse = "monthly",
              type = "xts")

library(readxl)
library(xts)
dat.1 <-data.frame(read_excel("C:/graphs/excel.data.xlsx"))
dates.1 <-as.Date(dat.l$Date, "%m/%d/%Y")
dat.a < xts(dat.1[,6], dates.1)
colnames(dat.a) <-c("SP500")

getSymbols("SPY",
           src = 'yahoo',
           from = '2014-12-31')

spy.1 <-Cl(SPY)                       # subset closing prices

write.table(spy.1,                    # file in R to export
            paste("C:/graphs/",       # file path address to hard drove
                  "spy.export",       # rename file (optional)
                  ".csv", sep = ""))  #add .csv suffix