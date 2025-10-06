library(dplyr)
library(fixest)
library(did)

dados <- readRDS("./output/data_did.rds")

#### Creating new Variables #####

dados2 <- dados %>% filter(mun_affected > 0)

###### Estimating 2SLS  - Without Controls #############
table_reg <- function(x){
  table1 <- broom::tidy(summary(x)) %>% filter(grepl('map_risco_inund|fisc_ench|sist_alert_ench|prog_hab_realoc',term)) %>% 
    mutate(estimate  = ifelse(length(round(`estimate`, 2)) == 1,round(`estimate`, 3),round(`estimate`, 2)),
           std.error = ifelse(length(round(`std.error`, 2)) == 1,round(`std.error`, 3),round(`std.error`, 2)),
           estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                       ifelse(p.value <= 0.05, paste0(estimate, "**"),
                       ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
           std.error = paste0("(",std.error,")")) %>% 
    select(c(Coef = estimate, " " = std.error,Stage1 = term)) %>% t
  
  controls <- ifelse(nrow(broom::tidy(summary(x, stage = 1))) > 2,"Yes","No")
  table <- rbind(table1,controls,x$nobs)
  return(table)
}

eq1 <- feols(share_desalojados ~ + 1 + map_risco_inund,
             se="hetero", data=dados2)

results1 <- table_reg(eq1)

eq2 <- feols(share_desalojados  ~ + 1 + fisc_ench,
             se="hetero", data=dados2)
results2 <- table_reg(eq2)


eq3 <- feols(share_desalojados  ~ + 1 + sist_alert_ench,
             se="hetero", data=dados2)
results3 <- table_reg(eq3)


###### Estimating 2SLS  - With Controls #############

eq1_control <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  + map_risco_inund,
                     se="hetero", data=dados2)
results1_control <- table_reg(eq1_control)

eq2_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  + fisc_ench,
                     se="hetero", data=dados2)
results2_control <- table_reg(eq2_control)


eq3_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  + sist_alert_ench,
                     se="hetero", data=dados2)
results3_control <- table_reg(eq3_control)


results <- cbind(results1,results1_control,
                 results2,results2_control,
                 results3,results3_control) %>% data.frame

names(results) <- paste0("(",gsub("X","",names(results)),")")
results
write.csv2(results,"./results/tables/02_effect_drm_disaster.csv",row.names =  F)
