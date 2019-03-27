
<!-- rnb-text-begin -->

---
title: "R Notebook"
output: html_notebook
---


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuc3VwcHJlc3NNZXNzYWdlcyhsaWJyYXJ5KHRpZHl2ZXJzZSkpXG5saWJyYXJ5KE1hdHJpeClcbmxpYnJhcnkoZnMpXG5saWJyYXJ5KG1hZ2ljaylcbnNvdXJjZShcImNvbndheS5SXCIpXG5gYGAifQ== -->

```r
suppressMessages(library(tidyverse))
library(Matrix)
library(fs)
library(magick)
source("conway.R")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubSA8LSBNYXRyaXg6OmJhbmRTcGFyc2UoMjAsMjAsMClcbmNsYXNzKG0pXG5kaW0obSlcbmBgYCJ9 -->

```r
m <- Matrix::bandSparse(20,20,0)
class(m)
dim(m)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubWF0cml4MmRmYSA8LSBmdW5jdGlvbihtKSBtICU+JVxuICBNYXRyaXg6OnN1bW1hcnkobSkgJT4lXG4gIGFzX3RpYmJsZVxuYGBgIn0= -->

```r
matrix2dfa <- function(m) m %>%
  Matrix::summary(m) %>%
  as_tibble
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxubSAlPiUgbWF0cml4MmRmYVxuYGBgIn0= -->

```r
m %>% matrix2dfa
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdF9tdHggPC0gZnVuY3Rpb24obSwuLi4pIHtcbiAgZGZfbSA8LSBtYXRyaXgyZGZhKG0pXG4gIGdncGxvdChkZl9tLCBhZXMoaSxqKSkgK1xuICAgIGdlb21fcmFzdGVyKGZpbGw9XCJibGFja1wiLC4uLikgK1xuICAgICNzY2FsZV9maWxsX21hbnVhbCh2YWx1ZXMgPSBjKFwid2hpdGVcIixcImJsYWNrXCIpKSArXG4gICAgY29vcmRfZml4ZWQoKSArXG4gICAgdGhlbWUobGVnZW5kLnBvc2l0aW9uID0gXCJub25lXCIpXG59XG5wbG90X210eChtKVxuYGBgIn0= -->

```r
plot_mtx <- function(m,...) {
  df_m <- matrix2dfa(m)
  ggplot(df_m, aes(i,j)) +
    geom_raster(fill="black",...) +
    #scale_fill_manual(values = c("white","black")) +
    coord_fixed() +
    theme(legend.position = "none")
}
plot_mtx(m)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucGxvdF9kZiA8LSBmdW5jdGlvbihkZix3aWR0aCwuLi4pIHtcbiAgZGYgJT4lXG4gICAgbXV0YXRlKGRlbnNpdHk9VCkgJT4lXG4gICAgZ2dwbG90KGFlcyhpLGopKSArXG4gICAgZ2VvbV90aWxlKGFlcyhmaWxsPWRlbnNpdHkpLGhlaWdodD0xLHdpZHRoPTEsLi4uKSArXG4gICAgY29vcmRfZml4ZWQoeGxpbT1jKDEsd2lkdGgpLHlsaW09YygxLHdpZHRoKSkgK1xuICAgICMgc2NhbGVfZmlsbF9tYW51YWwodmFsdWVzID0gYyhcImJsYWNrXCIsXCJ3aGl0ZVwiKSkgK1xuICAgICNjb29yZF9jYXJ0ZXNpYW4oeGxpbT1jKDFMLHdpZHRoKSx5bGltPWMoMUwsd2lkdGgpKSArXG4gICAgdGhlbWUobGVnZW5kLnBvc2l0aW9uID0gXCJub25lXCIsXG4gICAgICAgICAgYXhpcy50aXRsZSA9IGVsZW1lbnRfYmxhbmsoKSxcbiAgICAgICAgICBheGlzLnRleHQgPSBlbGVtZW50X2JsYW5rKCksXG4gICAgICAgICAgcGFuZWwuZ3JpZCA9IGVsZW1lbnRfYmxhbmsoKSxcbiAgICAgICAgICAjcGFuZWwuZ3JpZC5taW5vciA9IGVsZW1lbnRfYmxhbmsoKSxcbiAgICAgICAgICBheGlzLnRpY2tzID0gZWxlbWVudF9ibGFuaygpKVxufVxuYGBgIn0= -->

```r
plot_df <- function(df,width,...) {
  df %>%
    mutate(density=T) %>%
    ggplot(aes(i,j)) +
    geom_tile(aes(fill=density),height=1,width=1,...) +
    coord_fixed(xlim=c(1,width),ylim=c(1,width)) +
    # scale_fill_manual(values = c("black","white")) +
    #coord_cartesian(xlim=c(1L,width),ylim=c(1L,width)) +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          #panel.grid.minor = element_blank(),
          axis.ticks = element_blank())
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Generate random samples in matrix

<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxucmFuZG9tX2RmIDwtIGZ1bmN0aW9uKHdpZHRoLHNhbXBsZXMpIHtcbiAgdGliYmxlKGk9c2FtcGxlLmludCh3aWR0aCxzYW1wbGVzLHJlcGxhY2U9VCksXG4gICAgICAgICBqPXNhbXBsZS5pbnQod2lkdGgsc2FtcGxlcyxyZXBsYWNlPVQpKVxufVxuc3BhcnNlX3RvX2RmIDwtIGZ1bmN0aW9uKG0pIG0gJT4lIE1hdHJpeDo6c3VtbWFyeSgpICU+JSBhc190aWJibGUoKSAlPiUgc2VsZWN0KGksailcbmRmX3RvX3NwYXJzZSA8LSBmdW5jdGlvbihkZix3aWR0aCkge1xuICBtIDwtIE1hdHJpeChGLHdpZHRoLHdpZHRoLHNwYXJzZT1UKSAjXCJsc3BhcnNlTWF0cml4XCJcbiAgZGYgJT4lIHBtYXAofnttWy4uMSwuLjJdPDwtVH0pXG4gIG1cbn1cbmBgYCJ9 -->

```r
random_df <- function(width,samples) {
  tibble(i=sample.int(width,samples,replace=T),
         j=sample.int(width,samples,replace=T))
}
sparse_to_df <- function(m) m %>% Matrix::summary() %>% as_tibble() %>% select(i,j)
df_to_sparse <- function(df,width) {
  m <- Matrix(F,width,width,sparse=T) #"lsparseMatrix"
  df %>% pmap(~{m[..1,..2]<<-T})
  m
}
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGYxMCA8LSByYW5kb21fZGYoMTAsMTApXG5tMCA8LSBkZjEwICU+JSBkZl90b19zcGFyc2UoMTApXG5tMCAlPiUgc3BhcnNlX3RvX2RmKClcbmBgYCJ9 -->

```r
df10 <- random_df(10,10)
m0 <- df10 %>% df_to_sparse(10)
m0 %>% sparse_to_df()
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGYxMDAgPC0gcmFuZG9tX2RmKDEwMCwxMDAwKVxuZGYxMDAgJT4lIHBsb3RfZGYoMTAwKVxuYGBgIn0= -->

```r
df100 <- random_df(100,1000)
df100 %>% plot_df(100)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->




<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxudGliYmxlKGk9Yyg1LDEwLDE1KSxqPWMoNSwxMCwxNSksMjApJT4lXG4gIHBsb3RfZGYoMjApXG5gYGAifQ== -->

```r
tibble(i=c(5,10,15),j=c(5,10,15),20)%>%
  plot_df(20)
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->


Do simulation


<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuI20xMDAgPC0gZGYxMDAgJT4lIGRmX3RvX3NwYXJzZSgxMDApXG5zZXQuc2VlZCgwKVxuc2lkZTwtMTAwXG5kZnMgPC0gMTo2MCAlPiVcbiAgYWNjdW11bGF0ZSh+e3ByaW50KC55KTtjb253YXlfc3RlcCgueCxzaWRlKX0sXG4gICAgICAgICAgICAgLmluaXQ9cmFuZG9tX2RmKHNpZGUsKHNpZGVeMikvMTApKVxuYGBgIn0= -->

```r
#m100 <- df100 %>% df_to_sparse(100)
set.seed(0)
side<-100
dfs <- 1:60 %>%
  accumulate(~{print(.y);conway_step(.x,side)},
             .init=random_df(side,(side^2)/10))
```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiWzFdIDFcblsxXSAyXG5bMV0gM1xuWzFdIDRcblsxXSA1XG4ifQ== -->

```
[1] 1
[1] 2
[1] 3
[1] 4
[1] 5
```



<!-- rnb-output-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZGlyX2ZyYW1lcyA8LSBcImZyYW1lc1wiXG5pZihkaXJfZXhpc3RzKGRpcl9mcmFtZXMpKVxuICBkaXJfZGVsZXRlKGRpcl9mcmFtZXMpXG5kaXJfY3JlYXRlKGRpcl9mcmFtZXMpXG5cbmRmcyU+JWl3YWxrKH5nZ3NhdmUoc3ByaW50ZihcIiVzLyUwM2QucG5nXCIsZGlyX2ZyYW1lcywueSksXG4gICAgICAgICAgICAgICAgICAgICAgcGxvdF9kZigueCwxMDApKSlcbmBgYCJ9 -->

```r
dir_frames <- "frames"
if(dir_exists(dir_frames))
  dir_delete(dir_frames)
dir_create(dir_frames)

dfs%>%iwalk(~ggsave(sprintf("%s/%03d.png",dir_frames,.y),
                      plot_df(.x,100)))
```

<!-- rnb-source-end -->

<!-- rnb-output-begin eyJkYXRhIjoiU2F2aW5nIDcgeCA3IGluIGltYWdlXG4ifQ== -->

```
Saving 7 x 7 in image
```



<!-- rnb-output-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->





<!-- rnb-text-end -->


<!-- rnb-chunk-begin -->


<!-- rnb-source-begin eyJkYXRhIjoiYGBgclxuZnM6OmRpcl9scyhcImZyYW1lc1wiLHJlZ2V4cD1cIlxcXFxkK1xcXFwucG5nXCIpICU+JVxuICBtYXAoaW1hZ2VfcmVhZCkgJT4lXG4gIGltYWdlX2pvaW4oKSAlPiUgXG4gIGltYWdlX2FuaW1hdGUoZnBzPTIpICU+JSBcbiAgaW1hZ2Vfd3JpdGUoXCJjb253YXkuZ2lmXCIpXG5gYGAifQ== -->

```r
fs::dir_ls("frames",regexp="\\d+\\.png") %>%
  map(image_read) %>%
  image_join() %>% 
  image_animate(fps=2) %>% 
  image_write("conway.gif")
```

<!-- rnb-source-end -->

<!-- rnb-chunk-end -->


<!-- rnb-text-begin -->



<!-- rnb-text-end -->

