library(PerformanceAnalytics)
library(TTR)
library(quadprog)
library(xts)
library(fPortfolio)
library(fAssets)
library(DEoptimR)
library(timeSeries)

data(managers)

ret<-na.omit(managers[,5:10])

cov.mat <- cov(ret)
n.col <- ncol(cov.mat)
zero.mat <- array(0,dim = c(n.col,1))
one.mat <- matrix(1,nrow=n.col)
bvec.1 <- 1
meq.1 <- 1

mv.port <- solve.QP(Dmat = cov.mat,
                    dvec = zero.mat,
                    Amat = one.mat,
                    bvec = bvec.1,
                    meq = meq.1)

mv.port.wgt <- round(as.matrix(mv.port$solution) * 100, digits = 2)
rownames(mv.port.wgt) <-c(names(ret))
colnames(mv.port.wgt) <- c("MVP % weights")

mv.port.wgt

one.zero.diagonal <- cbind(1, diag(n.col))
bvec.1.vector <- as.matrix(c(1, rep(0,n.col)))


mv.port.noshort <- solve.QP(Dmat = cov.mat,
                    dvec = zero.mat,
                    Amat = one.zero.diagonal,
                    bvec = bvec.1.vector,
                    meq = meq.1)

mv.port.noshort.wgt <- round(as.matrix(mv.port.noshort$solution)
                             * 100,
                             digits = 2)

rownames(mv.port.noshort.wgt) <- c(names(ret))
colnames(mv.port.noshort.wgt) <- c("MVP % weights")
mv.port.noshort.wgt

one.zero.diagonal.a <- cbind(1,diag(n.col),
                             1 * diag(n.col),
                             -1* diag(n.col))

min.wgt <- rep(0.1, n.col)
max.wgt <- rep(0.5, n.col)

bvec.1.vector.a <- c(1,rep(0, n.col),
                     min.wgt,
                     -max.wgt)

mv.port.noshort.a <- solve.QP(Dmat = cov.mat,
                            dvec = zero.mat,
                            Amat = one.zero.diagonal.a,
                            bvec = bvec.1.vector.a,
                            meq = meq.1)

mv.port.noshort.wgt.a <- round(as.matrix(mv.port.noshort.a$solution)
                             * 100,
                             digits = 2)

rownames(mv.port.noshort.wgt.a) <- c(names(ret))
colnames(mv.port.noshort.wgt.a) <- c("MVP % weights")
mv.port.noshort.wgt.a


ret.ts <- as.timeSeries(na.omit(managers[,5:10]))

n.col <- ncol(ret.ts)

min.max <- c("minW[1:n.col]=0.1", "maxW[1:n.col]=0.5")

min.var.constraints <- minvariancePortfolio(ret.ts,constraints=min.max)
round(min.var.constraints@portfolio@portfolio$weights*100,2)

eff.frontier <-portfolioFrontier(ret.ts)

min.max.1 <- c("minW[1:n.col] = 0.10", 
               "maxW[1:n.col] = 0.50",
               "minsumW[c(1:3)] = 0.35",
               "maxsumW[c(1:3)] = 0.45")

min.var.constraints.1 <-minvariancePortfolio(ret.ts, constraints = min.max.1)

round(min.var.constraints.1@portfolio@portfolio$weights*100,2)