#---------------------------------------
# Simulation of confidence intervals and
# comparison to goodman
#---------------------------------------
source("benfordFunctions.R")
set.seed(20211230)
# Get data
# Read in the data downloaded from worldbank
d <- read.csv("data/exportData.csv",skip = 4)
# just get the numbers
z <- as.matrix(d[-(1:4)])
z <- z[!is.na(z)]
fs <- as.integer(fdigit(z))

#--------------------------------------
# simulation function for proportions
#--------------------------------------
simul_ci <- function(n, nsim, fs){
  res <- replicate(nsim,{
    s <- sample(fs,n)
    tb <- tabulate(s)
    tb / sum(tb)
  })
  # Correct the quantile to have a simultaneous 95% CI.
  tmp <- apply(res,1,function(x)c(quantile(x,probs = c(0.025/9,1 - (0.025/9))), 
                                  mean(x)))
  observed <- data.frame(
    digit = as.character(1:9),
    prop = tmp[3,],
    ll = tmp[2,],
    ul = tmp[1,] 
  )
  expect <- goodman(observed$prop*n)
  
  alldf <- rbind(observed,expect)
  alldf$obs <- rep(c("Simulated","Goodman"),
                   each = 9)
  return(alldf)
}

#--------------------------------------
# simulation for n=300 and n=1000
#--------------------------------------
# simulate for 300 
n <- 300
alldf <- simul_ci(n,1e5,fs)
write.csv(alldf,
          file = "data/simulci_300.csv",
          row.names = FALSE)

# simulate for 1000 
n <- 1000
alldf <- simul_ci(n,1e5,fs)
write.csv(alldf,
          file = "data/simulci_1000.csv",
          row.names = FALSE)
