library(PerformanceAnalytics)
library(TTR)
library(quadprog)
library(xts)
library(fPortfolio)
library(fAssets)
library(DEoptimR)
library(timeSeries)
library(quantmod)


download.file("http://bit.ly/2ikMUxn",
              destfile="F-F_Research_Data_Factors.zip", mode='wb')
unzip("F-F_Research_Data_Factors.zip")
ff.factors<-read.delim('F-F_Research_Data_Factors.txt',
                       sep="",
                       nrows=1067,
                       header=FALSE,
                       skip=4,
                       stringsAsFactors=FALSE)

names(ff.factors) <- c("Date", "MKT", "SMB", "HML", "RF")

ff.factors.3 <-ff.factors[,2:4]
dates.1 <-as.yearmon(as.character(ff.factors$Date), "%Y%m")

ff.factors.dates <-xts(as.matrix(ff.factors.3), as.Date(dates.1))

data(managers)
sp500.ret.tbill <-to.monthly(managers[,c(8,10)],
                             indexAt =  'firstof', OHLC = F)
sp500.ret.premium <-sp500.ret.tbill[,1] -sp500.ret.tbill[,2]
colnames(sp500.ret.premium) <-c("SP500")

sp500.dates <-as.Date(index(sp500.ret.premium))
ff.factors.subset <- ff.factors.dates[paste0(sp500.dates)]*0.01
ff.factors.sp500.ret.premium <- cbind(ff.factors.subset, sp500.ret.premium)

sp500.factors <-lm(SP500 ~., data=ff.factors.sp500.ret.premium)

summary(sp500.factors)

rp.estimate <-sp500.factors$coefficients[1] +
  sp500.factors$coefficient[2]*ff.factors.sp500.ret.premium$MKT +
  sp500.factors$coefficient[3]*ff.factors.sp500.ret.premium$SMB + 
  sp500.factors$coefficient[4]*ff.factors.sp500.ret.premium$HML

plot(as.numeric(sp500.ret.premium),
     as.numeric(rp.estimate),
     main="Fama French 3-factor model for S&P 500",
     ylab="estimated premia",
     xlab="historical premia")
grid()

ret <- na.omit(managers[,1:6])
fit <- factanal(ret, factors=1)

fit

plot(prcomp(ret))

ret.pc <- prcomp(ret)
print(summary(ret.pc),digits=3)

wgt.1 <- apply(ret.pc$rotation,2,function(x) x/sum(x))
round(wgt.1,4)

sum(wgt.1[,1])

symbols <- c("SPY", "AGG", "EFA", "EEM", "OIL")
getSymbols(symbols, src = "yahoo", auto.assign = T)
prices <- do.call(merge, lapply(symbols, function(x) Cl(get(x))))
colnames(prices) <-symbols

ret <- na.omit(ROC(prices['2007-12-31::2016-12-31'],
                   1,
                   "discrete",
                   na.pad = FALSE))

fit <-prcomp(ret)
factors.1 <-round(fit$rotation,3)
factors.1

wgt.a <- apply(fit$rotation,2,function(x) x/sum(x))
round(wgt.a[,1],2)
#newsymbols <-c("MDB", "CRWD", "ESTC")
#getSymbols(newsymbols, src = "yahoo", auto.assign = T)
#newprices <- do.call(merge, lapply(newsymbols, function(x) Cl(get(x))))
#colnames(newprices) <-newsymbols
#print(newprices)

