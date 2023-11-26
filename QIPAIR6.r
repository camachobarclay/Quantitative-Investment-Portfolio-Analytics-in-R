library(PerformanceAnalytics)
library(TTR)
library(quadprog)
library(xts)
library(fPortfolio)
library(fAssets)
library(DEoptimR)
library(timeSeries)
library(quantmod)
library(MASS)
library(mvtnorm)
library(QRM)
library(boot)

data(managers)
ret.sp500 <- managers$SP500 
set.seed(994)
ret.sp500.sim.1 <- sample(ret.sp500, replace = T)

ret.all <-cbind(ret.sp500, as.numeric(ret.sp500.sim.1))
matplot(ret.all, type ='l')

set.seed(645)
ret.sp500.sim.2 <-sample(as.numeric(ret.sp500),
                         size = 1000,
                         replace = T)
ret.a <- managers[,8:10]
set.seed(44)
ret.a.sim <-apply(ret.a,2, function(x) sample(x, replace = T))
ret.a.sim.1 <-xts(ret.a.sim, order.by = as.Date(index(ret.a)))

wgt.a <-c(0.6, 0.3, 0.1)
 
fun.a <- function(i,j) { # i = ret.a, j = wgt.a
   
   ret <- apply(i,2, function(x) sample(x, replace = T))
   ret.1 <- xts(ret, as.Date(index(i)))
   
   port.a <- Return.portfolio(ret.1,
                              rebalance_on="years",
                              weights = j,
                              wealth.index = T)
}

set.seed(48)
port.sim.test <- fun.a(ret.a, wgt.a)
head(port.sim.test)

set.seed(48)
port.sim.a <-do.call(cbind, lapply(1:5, function(x) fun.a(ret.a, wgt.a)))

port.actual <-Return.portfolio(ret.a,
                               rebalance_on = "years",
                               weights = wgt.a,
                               wealth.index=T)

wi.all <- cbind(port.actual, port.sim.a)

matplot(wi.all[,2:6],
        col = "gray",
        type ='l',
        ylab = "",
        main = "Wealth Indexes")

par(new = T)
matplot(wi.all[,1],
        col = "black",
        type = 'l',
        yaxt = "n",
        xaxt = "n",
        ylab = "",
        lwd = 2)
legend("topleft",
       c("simulated", "actual"),
       fill = c("gray", "black"))

set.seed(401)
ret <- rnorm(100, mean = 0.5, sd = 0.10)
prices <- cumprod(1 + ret)

prices.a <- cumprod(c(66.8,1 + ret))

mean.ret <- apply(managers[,8],2, mean)
mean.ret

sd.1<-apply(managers[,8],2,sd)
sd.1

set.seed(49)
sp500.sim <-rnorm(100, mean = mean.ret, sd = sd.1)
head(sp500.sim,3)

set.seed(49)

sp500.sim.a <-xts(rnorm(nrow(managers[,8]),
                        mean = mean.ret, sd = sd.1),
                        as.Date(index(managers[,8])))
head(sp500.sim.a)

set.seed(782)
assets.sim <- mvrnorm(n = nrow(managers[,8:10]),
                      mu = colMeans(managers[,8:10]),
                      Sigma = var(managers[,8:10]))

colMeans(managers[,8:10])
colMeans(assets.sim)

set.seed(23)
tdist.ret <- rt(100000, df = 13) * 0.01

spy.ret <- managers[,8]
spy.t.model <-fitdistr(spy.ret,"t")
spy.t.model

set.seed(77)
norm.ret <-rnorm(100000, mean = 0.05/12, sd = 0.15/sqrt(12))

t.kurt <- kurtosis(tdist.ret)
norm.kurt <- kurtosis(norm.ret)

t.kurt
norm.kurt

set.seed(77)
norm.ret <- rnorm(100, mean = 0.05/252, sd = 0.15/sqrt(252))
prices.a <-cumprod(1+ norm.ret)
plot(prices.a, type = "l", main = "Wealth Index")

fit <- fit.mst(managers[,8:10])
mu<- fit$mu
Sigma <- as.matrix(fit$Sigma)
nu <- fit$df
n <-nrow(managers[,8:10])

set.seed(198)
sim.dat <- rmvt(n = n, sigma = Sigma, df = nu, delta = mu)

sim.dat.xts <- xts(sim.dat, as.Date(index(managers[,8:10])))

ret.1<- managers[,c(1,3,4,8)]
colnames(ret.1) <-c("HF-A", "HF-B", "HF-c", "SP500")

coefficients.1 <- lm(ret.1$SP500 ~ ., data=ret.1)

coefficients.1

coef.1 <- function(equation, data, x){
        a <-data[x,]
        output <-lm(equation, data=a)
        return(coef(output))
}

set.seed(61)
results <- boot(data=ret.1,
                statistic=coef.1,
                R = 1000,
                equation=SP500 ~ .)

results.ci <-boot.ci(results, type = "basic", index = 2)
results.ci

plot(results, index = 2)