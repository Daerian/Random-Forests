mysamp <- function(n, m, s, lwr, upr, nnorm) {
  if(FALSE){
    '
    Function to generate specified number of samples from a normal distribution
    with mean m and variance s with an upper and lower limit. First generate 
    a number of samples without limit, and take a sub sample of that sample with 
    limits.
    
    n: number of samples to generate, between the specified limits
    m: mean of the normal distribution to be sampled from
    s: standard deviation of the normal distribution to be sampled from
    lwr: lower limit of the samples
    upr: upper limit of the samples
    nnorm: number of samples to generate from a normal distribution of mean m 
      and standard deviation s without any limits
    '
  }
  samp <- rnorm(nnorm, m, s)
  samp <- samp[samp >= lwr & samp <= upr]
  if (length(samp) >= n) {
    return(sample(samp, n))
  }  
  stop(simpleError("Not enough values to sample from. Try increasing nnorm."))
}

set.seed(42)
#Generate 50 random numbers from a normal distribution such that:
#The mean of the normal distribution is 50
#The variance is 100
#The lower limit is 20
#The upper limit is 40
#The 50 numbers taken from 1000 numbers without limit
sample <- mysamp(n=50, m=50, s=10, lwr=20, upr=40, nnorm=1000)
length(sample)
max(sample)
min(sample)
hist_breaks <- seq(from = 5, to = 45, by = 5) #Generate a list where each element is a break
hist(sample, breaks = hist_breaks) #Draw the histogram with custom breaks
