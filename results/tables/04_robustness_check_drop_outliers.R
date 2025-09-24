library(dplyr)
library(fixest)
library(did)

dados <- readRDS("./output/data_did.rds")

#### Creating new Variables #####

dados2 <- dados %>% filter(mun_affected == 1)
cemaden <- data.table::fread("./data/outras_bases/monitored_cemaden.csv")

Q <- quantile(dados2$desalojados, probs=c(.10, .90), na.rm = FALSE)
iqr <- IQR(dados2$desalojados)

up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

eliminated_95 <- subset(dados2, dados2$desalojados > (Q[1] - 1.5*iqr) & dados2$desalojados < (Q[2]+1.5*iqr))


Q <- quantile(dados2$desalojados, probs=c(.05, .75), na.rm = FALSE)
iqr <- IQR(dados2$desalojados)

up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range

eliminated_75 <- subset(dados2, dados2$desalojados > (Q[1] - 1.5*iqr) & dados2$desalojados < (Q[2]+1.5*iqr))
eliminated_non_desalojados <- subset(dados2, dados2$desalojados > 0)
eliminated_cemaden <- dados2 %>% filter(cemaden == 1)

#a <- dados2 %>% mutate(cemaden = ifelse(code_muni %in% cemaden$code,1,0))

# eq1_control <- feols(share_desalojados ~ cemaden + cobertura_agua +
#                        p_informal + p_urbana + log_gdp_pc + log_population + count_100mm ,
#                      se="hetero", data=a)

# summary(eq1_control)

###### Estimating 2SLS  - Without Controls #############
table_ivreg <- function(x){

  table2 <- broom::tidy(summary(x, stage = 2)) %>% filter(grepl('fit_',term)) %>% 
    mutate(estimate  = ifelse(length(round(estimate, 2)) == 1,round(estimate, 3),round(estimate, 2)),
           std.error = ifelse(length(round(std.error, 2)) == 1,round(std.error, 3),round(std.error, 2)),
           estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                       ifelse(p.value <= 0.05, paste0(estimate, "**"),
                       ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
           std.error  = paste0("(",round(std.error,2),")")) %>% 
    select(c(Coef = estimate, " " = std.error,Stage2 = term)) %>% t
  controls <- ifelse(nrow(broom::tidy(summary(x, stage = 1))) > 2,"Yes","No")
  fstat <- fitstat(x, "ivf")[[1]]$stat
  
  table <- rbind(table2,controls,round(fstat, 2),x$nobs)
  return(table)
}


###### Estimating 2SLS  - With Controls #############

eq1_control <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | map_risco_inund ~ z_mean_map_risco_inund,
                     se="hetero", data=eliminated_95)
results1_control <- table_ivreg(eq1_control)

eq2_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | fisc_ench ~ z_mean_fisc_ench,
                     se="hetero", data=eliminated_95)
results2_control <- table_ivreg(eq2_control)


eq3_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | sist_alert_ench ~ z_mean_sist_alert_ench,
                     se="hetero", data=eliminated_95)
results3_control <- table_ivreg(eq3_control)

eq4_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | prog_hab_realoc ~ z_mean_prog_hab_realoc,
                     se="hetero", data=eliminated_95)
results4_control <- table_ivreg(eq4_control)

results1 <- cbind(results1_control,
                  results2_control,
                  results3_control) %>% data.frame

##

eq1_control <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | map_risco_inund ~ z_mean_map_risco_inund,
                     se="hetero", data=eliminated_75)
results1_control <- table_ivreg(eq1_control)

eq2_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | fisc_ench ~ z_mean_fisc_ench,
                     se="hetero", data=eliminated_75)
results2_control <- table_ivreg(eq2_control)


eq3_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | sist_alert_ench ~ z_mean_sist_alert_ench,
                     se="hetero", data=eliminated_75)
results3_control <- table_ivreg(eq3_control)

eq4_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | prog_hab_realoc ~ z_mean_prog_hab_realoc,
                     se="hetero", data=eliminated_75)
results4_control <- table_ivreg(eq4_control)

results2 <- cbind(results1_control,
                  results2_control,
                  results3_control) %>% data.frame

##

eq1_control <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | map_risco_inund ~ z_mean_map_risco_inund,
                     se="hetero", data=eliminated_cemaden)
results1_control <- table_ivreg(eq1_control)

eq2_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | fisc_ench ~ z_mean_fisc_ench,
                     se="hetero", data=eliminated_cemaden)
results2_control <- table_ivreg(eq2_control)


eq3_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | sist_alert_ench ~ z_mean_sist_alert_ench,
                     se="hetero", data=eliminated_cemaden)
results3_control <- table_ivreg(eq3_control)

eq4_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | prog_hab_realoc ~ z_mean_prog_hab_realoc,
                     se="hetero", data=eliminated_cemaden)
results4_control <- table_ivreg(eq4_control)

results3 <- cbind(results1_control,
                 results2_control,
                 results3_control) %>% data.frame

##

eq1_control <- feols(share_desalojados ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | map_risco_inund ~ z_mean_map_risco_inund,
                     se="hetero", data=eliminated_non_desalojados)
results1_control <- table_ivreg(eq1_control)

eq2_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | fisc_ench ~ z_mean_fisc_ench,
                     se="hetero", data=eliminated_non_desalojados)
results2_control <- table_ivreg(eq2_control)


eq3_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | sist_alert_ench ~ z_mean_sist_alert_ench,
                     se="hetero", data=eliminated_non_desalojados)
results3_control <- table_ivreg(eq3_control)

eq4_control <- feols(share_desalojados  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm  | prog_hab_realoc ~ z_mean_prog_hab_realoc,
                     se="hetero", data=eliminated_non_desalojados)
results4_control <- table_ivreg(eq4_control)

results4 <- cbind(results1_control,
                  results2_control,
                  results3_control) %>% data.frame

df <- data.frame("." = c("Coef","","","Controls","KP F-Statistic - First Stage","Number of Observations"))
results1 <- cbind(df,results1)
rownames(results1) <- 1:nrow(results1)

results2 <- cbind(df,results2)
rownames(results2) <- 1:nrow(results2)

results3 <- cbind(df,results3)
rownames(results3) <- 1:nrow(results3)

results4 <- cbind(df,results4)
rownames(results4) <- 1:nrow(results4)


results <- rbind(results1,results2,results3,results4)

names(results) <- paste0("(",gsub("X","",names(results)),")")

write.csv2(results,"./results/tables/04_robustness_check_drop_outliers.csv",row.names =  F)



