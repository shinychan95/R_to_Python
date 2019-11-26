#m_10.R


library(mdsr)
library(tidyr)

NCI60 <- etl_NCI60()

NCI60

#h##
# set working directory
setwd("C:\\github\\R_to_Python\\R\\m_10")
write.csv(NCI60,file="NCI60.csv", row.names = FALSE)
####

Spreads <- NCI60 %>%
  gather(value = expression, key = cellLine, -Probe) %>%
  group_by(Probe) %>%
  summarize(N = n(), spread = sd(expression)) %>%
  arrange(desc(spread)) %>%
  mutate(order = row_number())
