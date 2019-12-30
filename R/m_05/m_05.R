library(mdsr) 

# set working directory
setwd("C:\\github\\R_to_Python\\R\\m_05")
write.csv(babynames,file="babynames.csv", row.names = FALSE)
  
library(dplyr)
library(babynames)
popular_names <- babynames %>% 
  group_by(sex, name) %>% 
  summarize(total_births = sum(n)) %>% 
  arrange(desc(total_births))

BP_narrow
