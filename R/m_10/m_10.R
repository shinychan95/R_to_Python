#m_10.R


library(mdsr)
library(tidyr)

NCI60 <- etl_NCI60()

NCI60

#h##
# set working directory
setwd("C:\\github\\R_to_Python\\R\\m_10")
write.csv(Violations,file="Violations.csv", row.names = FALSE)
####

Spreads <- NCI60 %>%
  gather(value = expression, key = cellLine, -Probe) %>%
  group_by(Probe) %>%
  summarize(N = n(), spread = sd(expression)) %>%
  arrange(desc(spread)) %>%
  mutate(order = row_number())

campus_sim <- function(num_sim = 1000, wait = 10) { 
  sally <- runif(num_sim, min = 0, max = 60) 
  joan <- runif(num_sim, min = 0, max = 60) 
  return(sum(abs(sally - joan) <= wait) / num_sim)
}

reps <- 5000 
params <- data.frame(num_sims = c(100, 400, 1600)) 

sim_results <- params %>%
  group_by(num_sims) %>%
  dplyr::do(mosaic::do(reps) * campus_sim(.$num_sims)) 

favstats(campus_sim ~ num_sims, data = sim_results)

sally <- runif(1000, min = 0, max = 60)
favstats(sally)

campus_sim()

sally <- runif(1000, min = 0, max = 60) 
joan <- runif(1000, min = 0, max = 60) 
table((abs(sally - joan) <= 10))["TRUE"]
sum(abs(sally - joan) <= 10)
