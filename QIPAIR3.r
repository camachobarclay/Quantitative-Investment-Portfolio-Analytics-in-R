library(PerformanceAnalytics)
library(TTR)
data(managers)
sp500.10yrT.ret <-managers[,8:9]

wgt.60.40 <-c(0.6,0.4)

port.60.40.rebal <-Return.portfolio(sp500.10yrT.ret,
                                    rebalance_on="years",
                                    weights = wgt.60.40,
                                    wealth.index = TRUE)

port.60.40.norebal <-Return.portfolio(sp500.10yrT.ret,
                                      weights = wgt.60.40,
                                      wealth.index = TRUE)


rebal.wi.1 <-cbind(port.60.40.rebal,
                   port.60.40.norebal)

colnames(rebal.wi.1) <-c("Rebal", "No Rebal")
matplot(rebal.wi.1,
        main = "Wealth Indezes",
        ylab = "",
        xlab = "",
        type = "l",
        lty = 1,
        col = c("black", "gray"))

legend(lty = c(1), "topleft",
       c(colnames(rebal.wi.1)),
       col=c("black", "gray"))

legend("bottomright", c("Dec. 31, 1995 = 1.0"))

rebal.wi.1.ret <-ROC(rebal.wi.1, n = 1, type = "discrete", na.pad = F)
port.60.40.ret <-Return.annualized(rebal.wi.1.ret, scale = 252)
port.60.40.sd <-apply(rebal.wi.1.ret, 2, sd)*sqrt(252)
port.60.40.sortino <-SortinoRatio(rebal.wi.1.ret)

risk.ret.60.40 <-round(rbind(port.60.40.ret,
                             port.60.40.sd,
                             port.60.40.sortino),3)

rownames(risk.ret.60.40) <-c("Annlz'd Return:",
                             "Annlz'd Volatility:",
                             "Annlz'd Sharpe Ratio:")

risk.ret.60.40

prices <- apply(managers[,8:9], 2, function(x) cumprod(1+x))
prices.xts <- xts(prices, order.by = index(managers[,8:9]))
sig.1 <- prices.xts/rollmean(prices.xts,k=10,align=c("right"))
sig.2 <- apply(sig.1,2,function(x) ifelse(x>1, yes=1, no=0))
sig.3 <- xts(sig.2, as.Date(index(tail(managers,nrow(sig.2)))))
sig.lag <- na.omit(lag(sig.3))

strategy.ret <- na.omit(managers[,8:9]*sig.lag)
strategy.wi <-cumprod(1+strategy.ret)

strategy.wi.wgt <-sweep(strategy.wi,
                        MARGIN = 2,
                        STATS = wgt.60.40,
                        FUN = "*")

taa.wi <-xts(apply(strategy.wi.wgt,1,sum),
            as.Date(index(strategy.wi.wgt)))

wi.3.ret <- na.omit(ROC(cbind(port.60.40.rebal,
                             port.60.40.norebal,
                             taa.wi),
                             1, "discrete"))

wi.3 <- xts(apply(wi.3.ret,2, function(x) cumprod(1+x)),
            as.Date(index(wi.3.ret)))
colnames(wi.3) <-c("Rebal", "No Rebel", "Tactical")

matplot(wi.3,
        main = "Wealth Indexes",
        ylab="",
        xlab="",
        type = "l",
        col=c("black", "gray", "black"))

legend(lty=c(1,1,3),"topleft",
       c(colnames(wi.3)),
       col=c("black","gray","black"))
legend("bottomright", c("Nov. 30, 1996 = 1.0"))

wi.3.monthly.ret <-ROC(wi.3,n=1,type="discrete", na.pad=F)
wi.3.annl.ret <-Return.annualized(wi.3.monthly.ret, scale=12)
wi.3.sd <-apply(wi.3.monthly.ret,2,sd)*sqrt(12)
wi.3.sr <-SharpeRatio.annualized(wi.3.monthly.ret)

risk.ret.wi.3 <-round(rbind(wi.3.annl.ret,
                            wi.3.sd,
                            wi.3.sr), 3)

rownames(risk.ret.wi.3) <-c("Annlz'd return:",
                            "Annlz'd Volatility:",
                            "Annlz'd Sharpe Ratio:")

risk.ret.wi.3