library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(bit64)

lu_bin <- c("0000","0001","0010","0011",
            "0100","0101","0110","0111",
            "1000","1001","1010","1011",
            "1100","1101","1110","1111")
lu_hex <- c("0123456789ABCDEF")%>%str_split("")%>%first

# create lookup table
lu <- new.env()
walk2(lu_bin,lu_hex,~{lu[[.x]] <- .y})

int2hex <- function(s) s%>%
  as.integer64.character%>%
  as.bitstring%>%
  str_sub(start=seq(1,61,4),end=seq(4,64,4))%>%
  map_chr(~lu[[.x]])%>%
  str_c(collapse="")

# from large int to bin
int2hex("601723167970426879")