get_neighs <- function(i,j)
  tibble(i=c(rep(i-1,3),rep(i,3),rep(i+1,3)),
         j=rep(c(j-1,j,j+1),3))

get_neighs0 <- function(i,j,width) get_neighs(i,j) %>%
  slice(-5) %>% # remove center cell
  filter(between(i,1L,width),
         between(j,1L,width))

count_on <- function(m,df) {
  df %>% pmap_int(~m[..1,..2]) %>% sum
}

conway_step <- function(df_m,width,min_neighs=2) { # sparse
  df_neighs <- df_m %>%
    pmap(get_neighs) %>%
    bind_rows %>%
    filter(between(i,1L,width),
           between(j,1L,width)) %>%
    distinct
  # for every (i,j) in df_neighs i want the
  # sum of neighbors which are on
  m <- df_m %>% df_to_sparse(width)
  df_keep <- df_neighs %>%
    pmap_lgl(~{count_on(m,get_neighs0(..1,..2,width))>=min_neighs})
  df_neighs %>% filter(df_keep)
}