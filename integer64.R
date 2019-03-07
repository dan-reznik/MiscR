library(bit64)

max64 <- as.integer64('9223372036854775807')
min64 <-  as.integer64('-9223372036854775807')
as.bitstring(c(min64,max64))
stringi::stri_length(as.bitstring(max64)) # 64 bits!

min64b <- -max64 # works
max64+1 # error
min64-1 # should work but doesn't