get_neighs <- function(i,j)
  tibble(i=c(rep(i-1,3),rep(i,3),rep(i+1,3)),
         j=rep(c(j-1,j,j+1),3))

get_neighs0 <- function(i,j,width) get_neighs(i,j) %>%
  slice(-5) %>% # remove center cell
  filter(between(i,1L,width),
         between(j,1L,width))

count_on <- function(df,m) {
  df %>% pmap_int(~m[..1,..2]) %>% sum
}

# Any live cell w/ (2,3) neighbors lives
# Any dead cell w/ 3 neighbors lives

count_neighs0 <- function(i,j,m) {
  width <- ncol(m)
  neighs <- get_neighs0(i,j,width) %>%
    count_on(m)
  neighs
}

conway_step <- function(df_m,width) { # sparse
  m <- df_m %>% df_to_sparse(width)
  live_cells <- df_m %>%
    mutate(neighs=pmap_int(.,count_neighs0,m)) %>%
    filter(neighs%in%c(2,3)) %>%
    mutate(status="live")
  
  dead_cells <- df_m %>% # which are neighboring
    #pmap(get_neighs0,width) %>% # anti_join eliminates lives
    pmap(get_neighs) %>%
    bind_rows %>%
    distinct %>%
    anti_join(df_m,by=c("i","j")) %>%
    # not needed if use "get_neighs0" above
    filter(between(i,1L,width),
           between(j,1L,width)) %>%
    mutate(neighs=pmap_int(.,count_neighs0,m)) %>%
    filter(neighs==3) %>%
    mutate(status="born")
  
  live_cells%>%bind_rows(dead_cells)%>%
    select(i,j)
}