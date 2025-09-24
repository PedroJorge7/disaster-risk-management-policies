library(dplyr)
library(fixest)
library(did)
library(AER)
library(ivreg)


dados <- readRDS("./output/data_did.rds")

#### Creating new Variables #####
library(tidyverse)
city = geobr::read_municipal_seat() %>% 
  extract(geom, c('lat', 'lon'), '\\((.*), (.*)\\)', convert = TRUE) %>% 
  select(c(code_muni,lat,lon))

dados2 <- dados %>% filter(mun_affected > 0) %>% 
  left_join(city)


#### Function

table_ivreg <- function(x){
  table1 <- broom::tidy(summary(x, stage = 1)) %>% filter(grepl('z_mean',term)) %>% 
    mutate(estimate  = ifelse(length(round(`estimate`, 2)) == 1,round(`estimate`, 3),round(`estimate`, 2)),
           std.error = ifelse(length(round(`std.error`, 2)) == 1,round(`std.error`, 3),round(`std.error`, 2)),
           estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                              ifelse(p.value <= 0.05, paste0(estimate, "**"),
                                     ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
           std.error = paste0("(",std.error,")")) %>% 
    select(c(Coef = estimate, " " = std.error,Stage1 = term)) %>% t
  
  
  table2 <- broom::tidy(summary(x, stage = 2)) %>% filter(grepl('fit_',term)) %>% 
    mutate(estimate  = ifelse(length(round(`estimate`, 2)) == 1,round(`estimate`, 3),round(`estimate`, 2)),
           std.error = ifelse(length(round(`std.error`, 2)) == 1,round(`std.error`, 3),round(`std.error`, 2)),
           estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                              ifelse(p.value <= 0.05, paste0(estimate, "**"),
                                     ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
           std.error  = paste0("(",std.error,")")) %>% 
    select(c(Coef = estimate, " " = std.error,Stage2 = term)) %>% t
  controls <- ifelse(nrow(broom::tidy(summary(x, stage = 1))) > 2,"Yes","No")
  fstat <- fitstat(x, "ivf")[[1]]$stat
  
  table <- rbind(table2,table1,controls,round(fstat, 2),x$nobs)
  return(table)
}

## Results
eq1_se1 <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | map_risco_inund ~ z_mean_map_risco_inund,
                     cluster = "code_muni",data=dados2)
results1_se1 <- table_ivreg(eq1_se1)


eq1_se3 <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | map_risco_inund ~ z_mean_map_risco_inund,
                     conley(50),
                     data=dados2)
results1_se3 <- table_ivreg(eq1_se3)

eq1_se4 <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | map_risco_inund ~ z_mean_map_risco_inund,
                     conley(100),
                     data=dados2)
results1_se4 <- table_ivreg(eq1_se4)

##
eq2_se1 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | fisc_ench ~ z_mean_fisc_ench,
                 cluster = "code_muni",data=dados2)
results2_se1 <- table_ivreg(eq2_se1)


eq2_se3 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | fisc_ench ~ z_mean_fisc_ench,
                 conley(50),
                 data=dados2)
results2_se3 <- table_ivreg(eq2_se3)

eq2_se4 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | fisc_ench ~ z_mean_fisc_ench,
                 conley(100),
                 data=dados2)
results2_se4 <- table_ivreg(eq2_se4)

##
eq3_se1 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | sist_alert_ench ~ z_mean_sist_alert_ench,
                 cluster = "code_muni",data=dados2)
results3_se1 <- table_ivreg(eq3_se1)


eq3_se3 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | sist_alert_ench ~ z_mean_sist_alert_ench,
                 conley(50),
                 data=dados2)
results3_se3 <- table_ivreg(eq3_se3)

eq3_se4 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | sist_alert_ench ~ z_mean_sist_alert_ench,
                 conley(100),
                 data=dados2)
results3_se4 <- table_ivreg(eq3_se4)

##
eq4_se1 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | prog_hab_realoc ~ z_mean_prog_hab_realoc,
                 cluster = "code_muni",data=dados2)
results4_se1 <- table_ivreg(eq4_se1)


eq4_se3 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | prog_hab_realoc ~ z_mean_prog_hab_realoc,
                 conley(50),
                 data=dados2)
results4_se3 <- table_ivreg(eq4_se3)

eq4_se4 <- feols(share_desalojados ~ cobertura_agua +
                   p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | prog_hab_realoc ~ z_mean_prog_hab_realoc,
                 conley(100),
                 data=dados2)
results4_se4 <- table_ivreg(eq4_se4)

##


results <- cbind(results1_se1,results1_se3,results1_se4,
                 results2_se1,results2_se3,results2_se4,
                 results3_se1,results3_se3,results3_se4) %>% data.frame

names(results) <- paste0("(",gsub("X","",names(results)),")")

df <- data.frame("." = c("","Coef","","","Coef","","Controls","KP F-Statistic - First Stage","Number of Observations"))

results <- cbind(df,results)

rownames(results) <- 1:nrow(results)
write.csv2(results,"./results/tables/04_robustness_check_alternative_ways_of_inference.csv",row.names =  F)
