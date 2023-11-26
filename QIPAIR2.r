library(PerformanceAnalytics)
library(TTR)
library(timeSeries)

data(managers)
head(managers, n=3)
sp500.ret <- managers$SP500
sp500.ret <-managers[,8]

managers[8,]

plot(sp500.ret*100,
     main = "S&P 500: 1-month % return",
     ylab = "return",
     xlab = "date")


sp500.index <-cumprod(1 + sp500.ret)
plot(sp500.index,
     main = "S&P 500 Index",
     ylab = "price",
     xlab = "date")

sp500.index <- cumprod(1+sp500.ret)
plot(sp500.index,
     main = "S&P 500 Index",
     ylab = "price",
     xlab = "date")

sp500.index <-cumprod(c(100,1+sp500.ret))


sp500.ret <- ROC(sp500.index,1, "discrete", na.pad = F)

data.test <-cbind(managers$SP500,sp500.ret)
colnames(data.test)[2] <-c("sp500.ret.alt")
head(data.test)

sp500.12mo.ret <- ROC(sp500.index, n = 12, "discrete", na.pad = F)

sp500.10yrT.ret <-managers[,8:9]*100
matplot(sp500.10yrT.ret,
        main = "S&P 500 vs. 10-Year Treasury",
        ylab="% return",
        type = 'l')

vol<-apply(managers[,8:9], MARGIN = 2, FUN = sd)
vol
vol.annualized = vol*sqrt(12)
vol.roll <-na.omit(rollapply(data=managers[,8:9],
                             width = 12, FUN = sd)*sqrt(12))
head(vol.roll)

corr.matrix <-cor(na.omit(managers[,8:9]))
corr.matrix

corr.roll <-na.omit(rollapply(na.omit(managers[,8:9]),
                              width = 12,
                              function(x) cor(x[,1], x[,2]),
                              by.column = FALSE))

head(corr.roll)

ret <- managers$SP500
round(StdDev(ret),5)
round(StdDev(ret, clean = c("geltner")),5)

round(StdDev.annualized(ret),5)

round(SharpeRatio.annualized(ret, Rf = 0.01/12), 3)

round(SortinoRatio(ret),3)

CAPM.beta(Ra=managers$EDHEC,Rb = managers$SP500)
coef(lm(formula=managers$EDHEC ~ managers$SP500))

qnorm(p = 0.05, mean = 0.07, sd = 0.10)
pnorm(q = -0.09448536, mean = 0.07, sd = 0.10)

qnorm(p = c(0.05, 0.01, 0.001), mean = 0.07, sd = 0.10)

set.seed(89)
ret.sim <-rnorm(10000, mean = 0.07, sd = 0.10)
ret.sim.var <- quantile(ret.sim, c(0.05, 0.01, 0.001))
ret.sim.var

quantile(managers$SP500, 0.05)

sp500.sort <-sort(as.numeric(managers$SP500))
sp500.sort.var95 <-quantile(sp500.sort, 0.05)

plot(sp500.sort,
     type = 'l',
     main = "Sorted S&P 500 Returns: 1996-2006",
     ylab = "monthly % return")

abline(h = sp500.sort.var95, lty = 2)
legend(lty = 2, "bottomright", c("5% quantile"))

prob.below.0 <- pnorm(q = 0,
      mean = mean(managers$SP500),
      sd = sd(managers$SP500),
      lower.tail=TRUE)

prob.above.0 <- pnorm(q = 0,
      mean = mean(managers$SP500),
      sd = sd(managers$SP500),
      lower.tail=FALSE)

sum(prob.below.0,prob.above.0)

table.Drawdowns(managers$SP500, top=3)
dd.history <-drawdowns(timeSeries(managers$SP500))
plot(dd.history*100,
     type="l",
     main = "S&P 500 Drawdowns: 1996-2006",
     ylab = "drawdown %")
