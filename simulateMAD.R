#---------------------------------------
# Simulation of MAD values 
#---------------------------------------
library(patchwork)
source("benfordFunctions.R")
set.seed(20211230)
library(DescTools) #Provides a random function for Benford
nsim <- 1e4
n<- 1000

expected <- dBenf(1:9)

mad <- replicate(nsim,{
  obs <- tabulate(rBenf(n), nbins = 9)
  observed <- obs/sum(obs)
  
  mean(abs(expected - observed))
})

vals <- c(100,200,500,1000,2000,5000)
res <- matrix(ncol = 2, nrow = length(vals) )
for(i in seq_along(vals)){
  n <- vals[i]
  mad <- replicate(nsim,{
    obs <- tabulate(rBenf(n),nbins = 9)
    observed <- obs/sum(obs)
    
    mean(abs(expected - observed))
  })
  res[i,] <- c(mean(mad),quantile(mad,0.95))
}
res <- cbind(res, res*sqrt(vals))
resdf <- as.data.frame(signif(t(res),2))
names(resdf) <- paste0("V",vals)
write.csv(resdf,
          file = "data/MADlimits.csv",
          row.names = FALSE)
