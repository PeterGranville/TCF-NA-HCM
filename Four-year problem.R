
library(tidyverse)

setwd("/Volumes/TOSHIBA EXT/Fed State Modeling/Model-V2/Simulation results")

f7 <- read.csv("F-7.csv", header=TRUE, check.names=FALSE)

index <- read.csv("Input combos.csv", header=TRUE, check.names=FALSE)

f7 <- left_join(x=f7, y=index, by="Policy index")

f7 <- f7 %>% filter(
  `Measure`=="Number of students participating", 
  `Value`=="0"
) %>% mutate(
  `Policy index` = as.numeric(
    gsub("F", "", `Policy index`)
  )
)
