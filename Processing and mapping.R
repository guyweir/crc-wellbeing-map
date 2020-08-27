# Mapping local area wellbeing

library(tidyverse)

#### Data prep ####

wb.data <- read.csv("ec5cc81b-85df-40c0-ba03-82dd96aaacc7.csv", stringsAsFactors = F)

#filter the data to most recent

max(wb.data$time)
wb.data <- wb.data %>% filter(`time` == max(time))
wb.data <- wb.data %>% filter(estimate == "Average (mean)")

wb.data <- wb.data %>% rename(`Score` = V4_3)

life.satisfaction <- wb.data %>% filter(allmeasuresofwellbeing == "Life Satisfaction")
anxiety <- wb.data %>% filter(allmeasuresofwellbeing == "Anxiety")
Happiness <- wb.data %>% filter(allmeasuresofwellbeing == "Happiness")
Worthwhile <- wb.data %>% filter(allmeasuresofwellbeing == "Worthwhile")
