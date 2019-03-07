library(tidyverse)
library(tictoc)
makeTibble <- function(n) tibble(random_real=runif(n),
                                 random_integer=sample.int(1000,size=n,replace=T))
tic()
10^(0:7) %>% map_int(~object.size(makeTibble(.))%>%as.integer)
toc()