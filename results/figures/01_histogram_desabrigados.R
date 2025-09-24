library(dplyr)
library(fixest)
library(did)
library(ggplot2)


dados <- readRDS("./output/data_did.rds")

#### Creating new Variables #####

dados2 <- dados %>% filter(mun_affected > 0) %>% 
  mutate( share_direct = (injured + deceased + sick + missing_people)/pop)