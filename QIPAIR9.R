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

symbols <-c(
"SPY",
"AGG",
"EFA",
"EEM",
"GSG",
"AOR")

getSymbols(symbols, src = "yahoo", auto.assign = T)
prices <-do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <-symbols

returns <-na.omit(ROC(prices['2010-12-31::2017-12-31'],
                      1,
                      "continuous",
                      na.pad = FALSE))

returns.m <- to.monthly(returns,
                        indexAt = 'firstof',
                        OHLC = F)
library(FactorAnalytics)

aor.style <-style.fit(R.funds = returns.m$AOR,
                      R.style = returns.m[, 1:5],
                      method = "constrained",
                      leverage = FALSE)

round(aor.style$weights,3)

regression.1 <- lm(returns.m$AOR~., data = returns.m)
summary(regeression.1)

style.roll.weights <-table.RollingStyle(R.fund = returns.m$AOR,
                                        R.style = returns.m[, 1:5],
                                        method = "constrained",
                                        leverage = FALSE)

round(head(style.roll.weights,3),3)
chart.RollingStyle(R.fund = returns.m$AOR,
                   main = "Style Weights For AOR",
                   R.style = returns.m[,1:5],
                   method = "constrained",
                   leverage = FALSE,
                   ylab = "weight")


symbols <- c("XLF",
             "XLK",
             "XLI",
             "XLB",
             "XLY",
             "XLV",
             "XLU",
             "XLP",
             "XLE",
             "VOX",
             "VNQ",
             "SPY")

getSymbols(symbols, src = "yahoo", auto.assign = T)
prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <- symbols

ret <-na.omit(ROC(prices['2006-12-31::2015-12-31'],
                  1,
                  "continuous",
                  na.pad = FALSE))

ret.m <-to.monthly(ret, 
                   indexAt = 'firstof',
                   OHLC = F)

# generate style weights
style.roll.weights.sp <-table.RollingStyle(R.fund = ret.m$SPY,
                                           R.style = ret.m[, 1:11],
                                           method = "constrained",
                                           leverage = FALSE)

#transform weights to end of quarter frequency
weights.1 <- to.quarterly(style.roll.weights.sp,
                          indexAt = 'lastof',
                          OHLC = F)

# create replicated SPY index with
#quarterly rebalancing.

spy.rep <- Return.portfolio(ret.m[, 1:11] ['2008/1'],
                            weights = weights.1,
                            wealth.index = TRUE,
                            verbose = TRUE) $wealthindex

# ret.m$SPY is trimmed to start in 2008
# to match weights.1, which uses 12/31/2007
# as the initial investment date

spy.actual <- cumprod(1+ret.m$SPY['2008/'])
# combine replicated and actual SPY indices
spy.rep.actual <-cbind(spy.rep, spy.actual)
colnames(spy.rep.actual) <-c("Replicated", "Actual")

matplot(spy.rep.actual,
        type = "1",
        lty = c(1,2),
        main = "SPY: Actual vs Replicated",
        col = "black")
legend("topright",
       lty = c(1,2),
       c(colnames(spy.rep.actual)),
       col = "black")

spy.rep.actual.ret <ROC(spy.rep.actual,1, "discrete", na.pad = F)
spy.actual.corr <- cor(spy.rep.actual.ret)