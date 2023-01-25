lapply(1:3, function(g) c(g, g^2, g^3))
lapply(1:3/3, function(g) round(g, digits=3))
library(parallel)

no_cores <- detectCores() - 1
cl <- makeCluster(no_cores)
parLapply(cl, 2:4,
          function(exponent)
            2^exponent)
stopCluster(cl)


cl<-makeCluster(no_cores)
base <- 2

parLapply(cl, 
          2:4, 
          function(exponent) 
            base^exponent)

stopCluster(cl)




cl<-makeCluster(no_cores)

base <- 2
clusterExport(cl, "base")
parLapply(cl, 
          2:4, 
          function(exponent) 
            base^exponent)

stopCluster(cl)


parSapply(cl, 2:4, 
          function(exponent) 
            base^exponent)

parSapply(cl, as.character(2:4), 
          function(exponent){
            x <- as.numeric(exponent)
            c(base = base^x, self = x^x)
          })


library(foreach)
install.packages('doParallel')
library(doParallel)

cl<-makeCluster(no_cores)
registerDoParallel(cl)

foreach(exponent = 2:4, 
        .combine = c)  %dopar%  
  base^exponent

foreach(exponent = 2:4, 
        .combine = rbind)  %dopar%  
  base^exponent


foreach(exponent = 2:4, 
        .combine = list,
        .multicombine = TRUE)  %dopar%  
  base^exponent

stopCluster()


base <- 2
cl<-makeCluster(2)
registerDoParallel(cl)
foreach(exponent = 2:4, 
        .combine = c)  %dopar%  
  base^exponent
stopCluster(cl)

test <- function (exponent) {
  foreach(exponent = 2:4, 
          .combine = c)  %dopar%  
    base^exponent
}
test()



log(-1)

message <- function(x) {
  if(x > 0)
    print('Hello')
  else
    print('Goodbye')
}

x <- log(-1)
message(x)

x <- 4
message(x)

message(log(-1))
traceback()

f <- function(x) {
  r <- x - g(x)
  r
}
g <- function(y) {
  r <- y * h(y)
  r
}
h <- function(z) {
  r <- log(z)
  if (r < 10)
    r^2
  else r^3
}

f(-1)

SS <- function(mu, x) {
  d <- x - mu
  d2 <- d^2
  ss <- sum(d2)
  ss
}

set.seed(100) ## set the RNG seed so that the results are reproducible
x <- rnorm(100)
SS(1, x)
debug(SS)
SS(2, x)
undebug(SS)
SS <- function(mu, x) {
  d <- x - mu
  d2 <- d^2
  browser()
  ss <- sum(d2)
  ss
}


set.seed(100)
p <- sort(runif(200))
thin <- rbinom(200, 1, p)
pp <- p[thin == 1]
hist(pp, nclass = 20)

nLL <- function(mu, x) {
  z <- mu * x
  lz <- log(z)
  trace("nLL", quote(if(any(is.nan(lz))) { browser() }), at=4, print=F)
  L1 <- sum(lz)
  L2 <- mu/2
  LL <- -(L1 - L2)
  LL
}

optim(100000, nLL, method = "BFGS", x = pp)
