
dat <- c(10,20,30)

x <- rnorm(1:100)


set.seed(154)

y <- rnorm(1:5)

set.seed(154)

z <- rnorm(1:10)

plot(z, type = 'l', panel.first=grid(), main = "Random Dataset")

1+1

10 - 3

3*4

100/4

a <- 12 + 5

a

dataset.1 <- 100
dataset.2 <- 50
dataset.1*dataset.2

set.seed(78)
matrix.dat <- data.frame(matrix(rnorm(15), ncol = 3))
matrix.dat

row.sum <- apply(matrix.dat, MARGIN = 1, FUN = sum)

matrix.dat.new <- cbind(matrix.dat, row.sum)

matrix.dat.new

colnames(matrix.dat.new)[1:3] <- c("A", "B", "C")
matrix.dat.new

colnames(matrix.dat.new) <- c("A", "B", "C", "D")

matrix.dat.new

rownames(matrix.dat.new) <- c("V", "W", "X", "Y", "Z")
matrix.dat.new

set.seed(132)
x <- data.frame(matrix(rnorm(6), ncol=2))

set.seed(98)

y<-rnorm(3)

y

x$new <- y

x