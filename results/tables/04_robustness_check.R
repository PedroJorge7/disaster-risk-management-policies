library(dplyr)
library(fixest)
library(did)

dados <- readRDS("./output/data_did.rds")

#### Creating new Variables #####

dados2 <- dados %>% filter(mun_affected == 1)

###### Estimating 2SLS  - Without Controls #############
table_ivreg <- function(x){
  table1 <- broom::tidy(summary(x, stage = 1)) %>% filter(grepl('z_mean|z_dum|z_sum|z_wmean',term)) %>% 
    mutate(estimate  = ifelse(length(round(`estimate`, 2)) == 1,round(`estimate`, 3),round(`estimate`, 2)),
           std.error = ifelse(length(round(`std.error`, 2)) == 1,round(`std.error`, 3),round(`std.error`, 2)),
           estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                              ifelse(p.value <= 0.05, paste0(estimate, "**"),
                                     ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
           std.error = paste0("(",round(std.error,2),")")) %>% 
    select(c(Coef = estimate, " " = std.error,Stage1 = term)) %>% t
  
  
  table2 <- broom::tidy(summary(x, stage = 2)) %>% filter(grepl('fit_',term)) %>% 
    mutate(estimate  = ifelse(length(round(`estimate`, 2)) == 1,round(`estimate`, 3),round(`estimate`, 2)),
           std.error = ifelse(length(round(`std.error`, 2)) == 1,round(`std.error`, 3),round(`std.error`, 2)),
           estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                              ifelse(p.value <= 0.05, paste0(estimate, "**"),
                                     ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
           std.error  = paste0("(",round(std.error,2),")")) %>% 
    select(c(Coef = estimate, " " = std.error,Stage2 = term)) %>% t
  controls <- ifelse(nrow(broom::tidy(summary(x, stage = 1))) > 2,"Yes","No")
  fstat <- fitstat(x, "ivf")[[1]]$stat
  
  table <- rbind(table2,table1,controls,round(fstat, 2),x$nobs)
  return(table)
}

###### Estimating 2SLS  - With Controls #############

# Eq 1
eq1_rob1 <- feols(share_desalojados ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    map_risco_inund ~ z_dum_map_risco_inund, se="hetero", data=dados2)
results1_rob1 <- table_ivreg(eq1_rob1)

eq1_rob2 <- feols(share_desalojados ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    map_risco_inund ~ z_sum_map_risco_inund, se="hetero", data=dados2)
results1_rob2 <- table_ivreg(eq1_rob2)

eq1_rob3 <- feols(share_desalojados ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    map_risco_inund ~ z_wmean_map_risco_inund, se="hetero", data=dados2)
results1_rob3 <- table_ivreg(eq1_rob3)




# Eq 2
eq2_rob1 <- feols(share_desalojados  ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    fisc_ench ~ z_dum_fisc_ench, se="hetero", data=dados2)
results2_rob1 <- table_ivreg(eq2_rob1)

eq2_rob2 <- feols(share_desalojados  ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    fisc_ench ~ z_sum_fisc_ench, se="hetero", data=dados2)
results2_rob2 <- table_ivreg(eq2_rob2)

eq2_rob3 <- feols(share_desalojados  ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    fisc_ench ~ z_wmean_fisc_ench, se="hetero", data=dados2)
results2_rob3 <- table_ivreg(eq2_rob3)






# Eq 3
eq3_rob1 <- feols(share_desalojados  ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    sist_alert_ench ~ z_dum_sist_alert_ench,se="hetero", data=dados2)
results3_rob1 <- table_ivreg(eq3_rob1)

eq3_rob2 <- feols(share_desalojados  ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    sist_alert_ench ~ z_sum_sist_alert_ench,se="hetero", data=dados2)
results3_rob2 <- table_ivreg(eq3_rob2)

eq3_rob3 <- feols(share_desalojados  ~ cobertura_agua + p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | 
                    sist_alert_ench ~ z_wmean_sist_alert_ench,se="hetero", data=dados2)
results3_rob3 <- table_ivreg(eq3_rob3)



results <- cbind(results1_rob2,results1_rob3,
                 results2_rob2,results2_rob3,
                 results3_rob2,results3_rob3) %>% data.frame

names(results) <- paste0("(",gsub("X","",names(results)),")")

df <- data.frame("." = c("","Coef","","","Coef","","Controls","KP F-Statistic - First Stage","Number of Observations"))

results <- cbind(df,results)

rownames(results) <- 1:nrow(results)
write.csv2(results,"./results/tables/04_robustness_check.csv",row.names =  F)
