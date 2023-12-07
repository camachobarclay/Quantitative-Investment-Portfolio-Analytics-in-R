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
fred.tickers <-c("RU1000TR", "MCOILWTICO") #download prices
getSymbols(fred.tickers, src = "FRED")

r1000.m <-to.monthly(RU1000TR, indexAt = 'firstof', OHLC = F)

#combine data sets and subset for 1986-2016 period
oil.r1000.a <-na.omit(cbind(MCOILWTICO,r1000.m))
oil.r1000 <-oil.r1000.a["1986::2016"]

oil.r1000.ret <- ROC(oil.r1000,1, "discrete", na.pad = F)
colnames(oil.r1000.ret) <-c("oil", "stocks")

var.lag <-VARselect(oil.r1000.ret) # select number of lags

# estimate VAR. Note that oil.r1000.ret is multiplied
# by 100 to generate percentages for viual convenience
# in the IRF graph. This adjustment only changes the scale of the results.

var.model < VAR(oil.r1000.ret*100, p=var.leg$selection[1])

plot(irf(var.model,
         impulse = "oil",
         response = "stocks",
         boot = TRUE,
         n,ahead = 12,
         cumulative = F),
     main = "Impulse Response From Oil")

irf.data <-irf(var.model,
               impulse = "oil",
               response="stocks",
               boot=T,
               n.ahead = 12,
               cumulative = F)

irf.data$irf      # point estimates
irf.data$Upper    # upper confidence band
irf.data$Lower    #lower confidence band

irf.num < unlist(irf.data$irf, use.names = F)

irf.data$irf$oil*2  # two std dev shock
irf.data$irf$oil*-1 # negative shock for one std dev

