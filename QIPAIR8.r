library(quantmod)
library(PerformanceAnalytics)
library(ROI)
library(PortfolioAnalytics)
library(quadprog)
library(ROI.plugin.quadprog)

library(Perc)

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

symbols<-c("SPY", "AGG", "EFA", "EEM", "GSG")
getSymbols(symbols, src = "yahoo", auto.assign = T)
prices <-do.call(merge,lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <- symbols

returns <- na.omit(ROC(prices['2007-12-31::2017-12-31'],
                       1,
                       "continuous",
                       na.pad = FALSE))

#set asset allocation weights

w = rep(x = 0.2, times = 5) # replicates 0.2 five times

#portfolio volatility (standard deviation)

port.vol <- sqrt(t(w) %*% cov(returns) %*% w)

#marginal contribution to risk
mcr <- w %*% cov(returns)/port.vol[1,1]

# component contribution to risk

cr <- mcr*w

# percent of contribution to overall portfolio risk
cr.port <- cr/port.vol[1,1]

# combine cr and cr.port
cr.all <-round(rbind(cr,cr.port),5)
rownames(cr.all) <-c("contribution to risk:", 
                     "% of risk contribution:")



cr.all


cr.1 <- StdDev(returns, weights = w, portfolio_method = "component")
cr.1

analyze.sd <- function(x,y) {#x =ret
  a <-StdDev(x, weights = y, portfolio_method = "component")
  b <-round(a$pct_contrib_StdDev,3)
  return(b)
}

cr.rolling <-na.omit(rollapply(returns, 
                                252,
                                function(x) analyze.sd(x,w),
                                by.column = FALSE,
                                align = "right"))

dat.1 <-cr.rolling*100
dat.2 <-dat.1[,order(-dat.1[nrow(dat.1),])] # order the results

matplot(dat.2,
        main = "Risk Contributions",
        ylab = "",
        xlab = "",
        type = "l",
        lwd = 2, 
        lty = c(1,1,3,3,5),
        col = c("black", "gray50", "black", "gray50", "black"))

legend("bottomleft",
       ncol = 1,
       lwd = 2,
       c(colnames(dat.2)),
       col = c("black", "gray50", "black", "gray50","black"),
       lty = c(1,1,3,3,5))

#library(FRAPO)

symbols <-c("SPY", "AGG", "EFA", "EEM", "GSG")
getSymbols(symbols, src = "yahoo", auto.assign = T)
prices <-do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <- symbols

returns <- na.omit(ROC(prices['2007-12-31::2017-12-31'],
                       1,
                       "continuous",
                       na.pad = FALSE))

covar.1 <- cov(returns) # calculate covariance matrix
risk.parity.w <- perc(covar.1) # risk parity weights
risk.parity.w

funds <-colnames(returns)
port.a<-portfolio.spec(asset = funds)
port.a <-add.constraint(portfolio = port.a,
                        type = "leverage",
                        min_sum = 0.99,
                        max_sum = 1.01)

port.a <- add.constraint(portfolio = port.a, type = "long_only")

port.a <-add.objective(portfolio = port.a,
                       type = "risk_budget",
                       min_concentration = TRUE,
                       name = "StdDev")

port.a1 <- optimize.portfolio(R = returns,
                              portfolio = port.a,
                              optimize_method = "quadprog",
                              trace = T)

round(extractWeights(port.a1)*100,3)

getSymbols("SPY", src = "yahoo", auto.assign=T)
spy.1 <-Cl(SPY["2010-12-31::2015-12-31"])

#function to generate SPY vol target weights
vol.target.w <-function(x,y){# x=asset price, y =risk factor
  a<- ROC(x,1, "discrete", na.pad=F)
  b<- rollapply(a, width =90, FUN=sd)*sqrt(252)
  c<- as.matrix(y/b)
  d<- 1/NCOL(x)
  e<-as.xts(ifelse(c>d,d,c))
  cash<-as.xts(1-(apply(e, MARGIN = 1, FUN = sum)))
  f<-merge(e,cash) 
  }

# create spy vol target weight histories
spy.5per.vol <-na.omit(vol.target.w(spy.1,0.05))
spy.10per.vol <- na.omit(vol.target.w(spy.1,0.10))

# extract month-end weights
w.5per <-to.period(spy.5per.vol,
                   period = 'months',
                   indexAt = 'lastof',
                   OHLC = F)

w.10per <-to.period(spy.10per.vol,
                    period = 'months',
                    indexAt = 'lastof',
                    OHLC = F)

spy.cash.ret <-cbind(ROC(spy.1,1, "discrete", na.pad = F), 0)
colnames(spy.cash.ret) <-c("spy", "cash")

port.w.5per <-Return.portfolio(spy,cash.ret,
                               weights=w.5per,
                               wealth.index=T,
                               verbose=TRUE)$wealthindex

colnames(port.w.5per) <-c("vol.target.5per")

port.w.10per <- Return.portfolio(spy.cash.ret,
                                 weight=w.10per,
                                 wealth.index=T,
                                 verbose =TRUE) $wealthindex

colnames(port.w.10per) <-c("vol.target.10per")

spy.port <cumprod(1+spy.cash.ret$spy["2011-06-01/"])

port.all <-cbind(spy.port, port.w.10per, port.w.5per)

matplot(port.all,
        main = "SPY vs. Volatility Target Strategies",
        ylab= "",
        xlab = "",
        type = "1",
        lwd = 2,
        lty = c(1,1,3),
        col = c("black", "gray50", "black"))

legend("bottomright",
       ncol = 1,
       lwd = 2,
       c(colnames(port.all)),
       col = c("black", "gray50", "black"),
       lty = c(1,1,3))

port.all.ret<-ROC(port.all,1,"discrete", na.pad=F)
port.all.vol <-apply(port.all.ret,2,sd)* sqrt(252)
round(port.all.vol,3)