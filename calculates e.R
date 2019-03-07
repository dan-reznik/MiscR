library(tidyverse)
library(compiler)

# Sale's "spigot" algm (1968), https://goo.gl/nm7HRL

# Estimates m!
#stirling <- function(m) m*(log(m)-1)+.5*log(6.2831852*m)
# log only once
log2pi <- .5*log(2*pi)
stirling <- function(m) log(m)*(m+.5)-m+log2pi

# n! ~ sqrt(2*pi*n)*(n/e)^n
# e^-n ~ n!/sqrt(2*pi*n)*n^-n
# e^n ~ sqrt(2*pi*n)/n!*n^n
# e ~ sqrt(2*pi*n/n!)^(1/n)*n
etry <- function(n) n*(sqrt(2*pi*n)/factorial(n))^(1/n)
data_frame(n=3:30,e=etry(n))%>% # factorial can go to 170
  ggplot(aes(n,e)) +
  geom_line() +
  geom_hline(yintercept = exp(1),linetype=2,color="red") +
  ggtitle("e approx vs n")
  #scale_x_continuous(breaks=seq(1,170))

# shows whether stirling approx works
data_frame(i=1:10,fact=log(factorial(i)),stir=stirling(i)) %>%
  gather(type,val,fact,stir) %>%
  ggplot(aes(x=i,y=val,group=type,color=type)) +
  geom_line() + geom_point() +
  scale_x_continuous(breaks=seq(1,10))

# Search for smallest coefficient vector length
find_m <- function(n) {
  test <- (n + 1L)*2.30258509
  m <- 5L
  while(stirling(m) <= test) m <- m + 1L
  m
}

find_m_comp <- cmpfun(find_m)

# Calculates "e" to n digits, returns as int array
# looks like its a n*m = n^2*log(n) algm
calc_e <- function(n) {
  n <- as.integer(n)
  m <- find_m_comp(n)
  coef <- rep(1L,m) # pre-allocates coefficients
  d <- rep(0L,n+1L) # pre-allocates digits
  d[1L] <- 2L # first digit is 2
  for (i in 1L:n) {
    carry <- 0L
    for (j in m:2L) {
      temp <- coef[j] * 10L + carry
      carry <- temp %/% j # integer div
      coef[j] <- temp - carry * j
    }
    d[i+1L] <- carry
  }
  d
}

calc_e_comp <- cmpfun(calc_e)

# its<-1000L
# microbenchmark(
#   calc_e(its),
#   calc_e_comp(its),
#   times=10
# )

# Converts int array to string
calc_e_s <- function(n) {
  es <- str_c(calc_e_comp(n),collapse="")
  str_c(str_sub(es,end=1L),
        str_sub(es,start=2L),sep=".")
}

# Test it
calc_e_s(200L)
