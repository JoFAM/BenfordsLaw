#--------------------------------------------------
# Functions for use to illustrate Benford's law
#--------------------------------------------------

#--------------------------------------------------
# fdigit: Get the first digit from a set of numbers
#--------------------------------------------------
fdigit <- function(x, na.rm = TRUE){
  if(na.rm){
    x <- x[!is.na(x)]
  } else if(any(is.na(x))){
    warning("Missing values will mess up your proportion table!")
  }
  trunc(x/ 10^floor(log10(x)))
}

#--------------------------------------------------
# create_sample: Random sample generator with cutoff
#--------------------------------------------------
# This function works only for positive numbers. It will
# assume a uniform distribution for the mantisse, and 
# multiplies this with a range of 10-folds derived from
# the given minimum and maximum. Then it will generate 
# randomly new numbers for every number that falls outside
# the limits until the sample satisfies conditions.
create_sample <- function(n, min = 1, max = 20000,
                          prob = NULL){
  if(any(c(min,max) < 0))
    stop("This only works for positive numbers")
  else if(min == 0) 
    min <- min + 10^(-15)
  
  
  # min & max
  lmin <- floor(log10(min))
  lmax <- ceiling(log10(max))
  es <- seq(lmin,lmax,by=1)
  
  # make sample  
  s <- runif(n)
  e <- sample(es,n,replace = TRUE,
              prob = prob)
  
  samp <- 10^s * 10^e
  while(any(id <- samp < min | samp > max)){
    nnew <- sum(id)
    s <- runif(nnew)
    e <- sample(es,nnew,replace = TRUE)
    samp[id] <- 10^s * 10^e
  }
  samp
}

#--------------------------------------------------
# Goodman simultaneous intervals 
#--------------------------------------------------
# Need a table as input
goodman <- function(nx, a = 0.05){
  n <- sum(nx)
  k <- length(nx)
  B <- qchisq((1-a/k), 1) # Xa,1 bcs a is right tail!
  fx <- as.vector(nx)
  
  dev <- sqrt(B*(B + 4*fx*(n-fx)/n))
  ul <- (B + 2*fx + dev)/(2*(n + B))
  ll <- (B + 2*fx - dev)/(2*(n + B))
  
  data.frame(
    digit = as.character(1:9),
    prop = as.vector(prop.table(nx)),
    ll = ll,
    ul = ul
  )
}

#--------------------------------------------------
# Plot for distributions 
#--------------------------------------------------
library(ggplot2)
library(DescTools)

plotbenford <- function(x, add.expected = TRUE){
  df <- goodman(tabulate(fdigit(x)))
  expect <- goodman(dBenf(1:9)*length(x))
  
  df <- rbind(df,expect)
  df$obs <- rep(c("observed","expected"),
                each = 9)
  
  ggplot(df, aes(x = digit)) +
    geom_crossbar(aes(y = prop,
                      ymin = ll,
                      ymax = ul,
                      fill = obs),
                  position = position_dodge2(width = 0.4, padding = 0)) 
}

#--------------------------------------------------
# Calculate corrected MAD 
#--------------------------------------------------
# Takes a sample x and 
madbenford <- function(o,p, n = 1){
  mean(abs(p - o))*sqrt(n)
}
