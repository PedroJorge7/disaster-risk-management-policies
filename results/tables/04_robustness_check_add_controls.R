library(dplyr)
library(fixest)
library(did)

dados <- readRDS("./output/data_did.rds")

#### Creating new Variables #####

dados2 <- dados %>% filter(mun_affected == 1) %>% 
  mutate(cobertura_florestamento = ifelse(is.na(cobertura_florestamento),0,cobertura_florestamento))

###### Estimating 2SLS  - Without Controls #############
table_ivreg <- function(x){
  table1 <- broom::tidy(summary(x, stage = 1)) %>% filter(grepl('z_mean',term)) %>% 
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

dados2$log_despesas <- log(dados2$despesa+1)

###### Estimating 2SLS  - With Controls #############

eq1_control <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  + agua_esgoto + log_despesas + cobertura_florestamento | map_risco_inund ~ z_mean_map_risco_inund,
                     se="hetero", data=dados2)
results1_control <- table_ivreg(eq1_control)

eq2_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  + agua_esgoto + log_despesas + cobertura_florestamento | fisc_ench ~ z_mean_fisc_ench,
                     se="hetero", data=dados2)
results2_control <- table_ivreg(eq2_control)


eq3_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  + agua_esgoto + log_despesas + cobertura_florestamento | sist_alert_ench ~ z_mean_sist_alert_ench,
                     se="hetero", data=dados2)
results3_control <- table_ivreg(eq3_control)

eq4_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  + agua_esgoto + log_despesas + cobertura_florestamento | prog_hab_realoc ~ z_mean_prog_hab_realoc,
                     se="hetero", data=dados2)
results4_control <- table_ivreg(eq4_control)

results <- cbind(results1_control,
                 results2_control,
                 results3_control) %>% data.frame

names(results) <- paste0("(",gsub("X","",names(results)),")")

df <- data.frame("." = c("","Coef","","","Coef","","Controls","KP F-Statistic - First Stage","Number of Observations"))

results <- cbind(df,results)

rownames(results) <- 1:nrow(results)
write.csv2(results,"./results/tables/04_robustness_check_add_controls.csv",row.names =  F)
