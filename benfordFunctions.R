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
  
  # First put in scientific notation
  fx <- format(as.numeric(x), # integers don't format correctly
               scientific = TRUE)
  
  substr(fx,1,1)
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
create_sample <- function(n, min = 1, max = 20000){
  if(any(c(min,max) < 0))
    stop("This only works for positive numbers")
  else if(min == 0) 
    min <- min + 10^(-15)
  
  # min & max
  lmin <- floor(log10(min))
  lmax <- ceiling(log10(max))
  es <- seq(lmin,lmax,by=1)
  s <- runif(n)
  e <- sample(es,n,replace = TRUE)
  
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
goodman <- function(x, a = 0.05){
  nx <- table(x)
  n <- sum(nx)
  k <- length(nx)
  B <- qchisq((1-a)/k, 1) # Xa,1 bcs a is right tail!
  fx <- as.vector(nx)
  
  dev <- sqrt(B*(B + 4*fx*(n-fx)/n))
  ul <- (B + 2*fx + dev)/(2*(n + B))
  ll <- (B + 2*fx - dev)/(2*(n + B))
  
  data.frame(
    digit = names(nx),
    prop = as.vector(prop.table(nx)),
    ll = ll,
    ul = ul
  )
}
