library(tidyverse)

make_m <- function(th) {
  cth<-cos(th)
  sth<-sin(th)
  m<-matrix(c(sth,cth,cth,-sth),byrow=T,ncol=2)
  m
}
m <- make_m(.01)
m %*% c(1,0)

make_polygon <- function(sides) {
  th<-2*pi/sides
  m <- make_m(th)
  accumulate(1:(sides-1),~{m%*%.x},.init=c(1,0))
}

poly_df <- function(poly) poly %>%
  map_dfr(~tibble(x=.x[[1]],y=.x[[2]]))

pentagon <- make_polygon(5) %>% poly_df
