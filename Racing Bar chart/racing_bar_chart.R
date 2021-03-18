### Loading libraries
library(tidyverse)
library(ggplot2)
library(gganimate)
library(RColorBrewer)

### Loading Data
data <- read.csv('https://gist.githubusercontent.com/johnburnmurdoch/2e5712cce1e2a9407bf081a952b85bac/raw/08cf82f5e03c619f7da2700d1777da0b5247df18/INTERBRAND_brand_values_2000_2018_decimalised.csv')
View(data)
 
###     
data %>%
  filter(rank<=10) %>% 
  ggplot(aes(x=-rank,y=value,fill=name, group=name)) +
  geom_tile(aes(y=value/2,height=value),width=0.9,show.legend = F)+
  geom_text(aes(label=name),
      hjust="right",
      colour="black",
      fontface="bold",
      nudge_y=-1000)+
  geom_text(aes(label=scales::comma(value)),
      hjust="left",
      nudge_y=2000,
      colour="grey30")+
  theme_minimal() +
  coord_flip(clip="off") +
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma)+
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        axis.text.y=element_blank()) +
  transition_time(year)-> p
      
animate(p, nframes = 300, fps = 5, end_pause = 20)
