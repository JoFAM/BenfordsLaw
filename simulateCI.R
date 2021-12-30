#---------------------------------------
# Simulation of confidence intervals and
# comparison to goodman
#---------------------------------------
library(patchwork)
source("benfordFunctions.R")
set.seed(20211230)
# Get data
# Read in the data downloaded from worldbank
d <- read.csv("data/exportData.csv",skip = 4)
# just get the numbers
z <- as.matrix(d[-(1:4)])
z <- z[!is.na(z)]
fs <- as.integer(fdigit(z))

# simulation function
simul_ci <- function(n, nsim, fs){
  res <- replicate(nsim,{
    s <- sample(fs,n)
    tb <- tabulate(s)
    tb / sum(tb)
  })
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

# simulate for 300 
n <- 300
alldf <- simul_ci(n,1e5,fs)
write.csv(alldf,
          file = "data/simulci_300.csv",
          row.names = FALSE)

p1 <- ggplot(alldf, aes(x = digit)) +
  geom_crossbar(aes(y = prop,
                    ymin = ll,
                    ymax = ul,
                    fill = obs),
                position = position_dodge2(width = 0.4, padding = 0)) +
  labs(fill = "Confidence\ninterval",
       y = "proportion",
       title = "Sample of 300 observations")

# simulate for 1000 
n <- 1000
alldf <- simul_ci(n,1e5,fs)
write.csv(alldf,
          file = "data/simulci_1000.csv",
          row.names = FALSE)

p2 <- ggplot(alldf, aes(x = digit)) +
  geom_crossbar(aes(y = prop,
                    ymin = ll,
                    ymax = ul,
                    fill = obs),
                position = position_dodge2(width = 0.4, padding = 0)) +
  labs(fill = "Confidence\ninterval",
       y = "proportion",
       title = "Sample of 1,000 observations")

p1 + p2 +
  plot_layout(guides = "collect") +
  plot_annotation("Comparison of 95% confidence intervals")
