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
options(show.error.locations = TRUE)

disk.loc.1 <- "/Users/frankiecamacho/Library/CloudStorage/Box-Box/R_Scripts"
data(managers)
sp500.ret <- managers$SP500['2005']*100

tiff(file = file.path(disk.loc.1, paste("fig.11.1",
                                        Sys.Date(),
                                        ".tiff", sep="")),
     units = "in", width = 4.5, height = 4, res = 1000)
plot(sp500.ret,
     main = "S&P 500: 1-month % returns",
     ylab = "return",
     xlab = "date")


sp500.ret <- managers$SP500['2005-01::2006-01-31']*100
dates.1 <- as.Date(index(sp500.ret))
dates.2 <- seq(first(dates.1), last(dates.1), by = "3 month")

ticks.a <-pretty(range(sp500.ret))

plot(dates.1,
     sp500.ret,
     type = 'l',
     main = "S&P 500: monthly returns",
     cex.main = 0.9,
     ylab = "returns",
     xlab="",
     yaxt = "n",
     xaxt = "n",
     ylim = c(min(ticks.a), max(ticks.a)),
     panel.first = c(abline(v = dates.2, col = "gray", lty = 3),
                     abline(h = ticks.a, col = "gray", lty = 3)))

axis(side = 2, las = 2, at = ticks.a, lab = paste0(ticks.a, "%"))
axis.Date(side = 1, dates.1, at = dates.2, format = "%b - %Y")
box()


sp500.ret <- managers$SP500['2005-01-31::2006-01-31']
sp500.ret.1 <-to.monthly(sp500.ret, indexAt = 'firstof', OHLC = F)
sp500.ret.df <-data.frame(sp500.ret.1)
sp500.ret.df$date <-as.Date(index(sp500.ret.1))

ggplot(sp500.ret.df) + geom_line(aes(x = sp500.ret.df$date, y = SP500.TR))+
  scale_x_date(breaks = date_breaks('1 year'), 
               labels =date_format("%b-%Y")) +
  scale_y_continuous(labels = percent) + 
  labs(title = "S&P 500: monthly. returns") + 
  ylab(NULL) +
  xlab(NULL) +
  theme(text = element_text(size = 9)) + 
  theme(panel.background = element_rect(fill = 'gray'))