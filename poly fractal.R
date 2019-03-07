library(tidyverse)
library(stringr)
library(purrr)

rotMtx <- function(th) {
  s <- sin(th)
  c <- cos(th)
  matrix(c(c,s,-s,c),ncol=2)
}

rotVec <- function(m,v) (m %*% v)[,1]

getVtx <- function(n) {
  m <- rotMtx(2*pi/n)
  # aplica rotacao repetidamente, usando ultimo valor.
  accumulate(1:(n-1),
             ~rotVec(m,.x),
             .init=c(0,1))
}

# gera lista com m vertices (de 1:n) sem repeticao consecutiva
getRandVtx <- function(m,n) { 
  rs <- sample.int(n-1,size=m,replace=T)
  inds <- 1:n
  # for each iteration:
  # (1) remove previous choice (.x ~ 1:n) from indices (inds)
  # (2) select from remainder a random item (.y ~ 1:n-1)
  #accumulate(rs,~setdiff(range,.x)[.y])
  accumulate(rs,~inds[-.x][.y] #,.init=1
             )
}

midPoint <- function(a,b) .5*(a+b)
list2DFold <- function(l) data.frame(t(matrix(unlist(l),nrow=2)))
list2DF <- function(l) do.call(tribble,c(~x, ~y, unlist(l)))

getMidPts <- function(m,n) { 
  poly <- getVtx(n) # returns a list
  vtx <- getRandVtx(m,n) # returns an int vector
  theAcc <- accumulate(vtx,
                       ~midPoint(.x,poly[[.y]]),
                       .init=c(0,0))
  # add column w random vtx indices for coloring
  list2DF(theAcc) %>% mutate(vtx=c(NA,vtx))
}

## poly vtx
myN<-5
## calculated midpoints
myV<-1e5

polyDF <- list2DF(getVtx(myN))
midPts <- getMidPts(myV,myN)

## Plot

polyDF %>% mutate(polyI=row_number()) %>%
  ggplot(aes(x=x,y=y)) +
  geom_point(aes(color=factor(polyI),size=.05)) +
  geom_polygon(fill=NA,color="black") +
  geom_point(data=midPts,aes(x=x,y=y,color=factor(vtx)),size=.01) +
  coord_fixed() +
  theme(legend.position="none")
