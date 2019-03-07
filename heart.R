library(tidyverse)
df <- data_frame(t=seq(-pi,0,pi/20),
                 x1=16*sin(t)^2,
                 x2=-x1,
                 y=14*cos(t)-7*cos(2*t)-2*cos(3*t)-2*cos(4*t)
) %>%
  gather(side,x,x1,x2) %>% 
  mutate_at(vars(x,y),~round(.,3)) %>%
  mutate(row=row_number())

df %>%
  ggplot(aes(x=x,y=y,label=as.character(row))) +
  geom_polygon(fill="red") +
  geom_point(color="black") +
  geom_path() +
  ggrepel::geom_label_repel(color="black", size= 5, nudge_y = 1) +
  coord_fixed() +
  theme_minimal()


df %>%
  #slice(side) %>% 
  ggplot(aes(x=x,y=y,fill=side,label=as.character(row))) +
  geom_polygon() +
  geom_point(color="black") +
  geom_path() +
  ggrepel::geom_label_repel(color="black", fill="white",size= 5, nudge_y = 1) +
  coord_fixed() +
  theme_minimal()

df %>%
  mutate(x=x+if_else(side=="x1",2,-2),row=row_number()) %>%
  ggplot(aes(x=x,y=y,label=row)) +
  geom_polygon(aes(fill=side)) +
  geom_point(color="black") +
  geom_path() +
  ggrepel::geom_label_repel(color="black", fill="white",size= 5, nudge_y = 1) +
  coord_fixed() +
  theme_minimal()