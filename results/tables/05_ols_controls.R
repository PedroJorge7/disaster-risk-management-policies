library(dplyr)
library(fixest)
library(did)

dados <- readRDS("./output/data_did.rds")

#### Creating new Variables #####

dados2 <- dados# %>% filter(mun_affected > 0)

###### Estimating 2SLS  - Without Controls #############
table_reg <- function(x){
  table1 <- broom::tidy(summary(x)) %>% 
    mutate(estimate  = round(estimate, 3),
           std.error = round(std.error, 3),
           estimate  = ifelse(p.value <= 0.01, paste0(estimate, "***"),
                       ifelse(p.value <= 0.05, paste0(estimate, "**"),
                       ifelse(p.value <= 0.10, paste0(estimate, "*"), estimate))),
           std.error = paste0("(",std.error,")")) %>% 
    select(c(term,Coef = estimate, std.error))
  
  # Invertendo a ordem das linhas do dataframe
  table1 <- table1[rev(rownames(table1)), ]
  
  table1 <- do.call(rbind, lapply(1:nrow(table1), function(i) {
    rbind(data.frame(term = table1$term[i], Coef = table1$Coef[i], stringsAsFactors = FALSE),
          data.frame(term = "", Coef = table1$std.error[i], stringsAsFactors = FALSE))
  }))
  
  stats <- fitstat(x, c("n", "r2", "f"))
  
  # controls <- ifelse(nrow(broom::tidy(summary(x, stage = 1))) > 2,"Yes","No")
  table <- rbind(
    table1,
    data.frame(term = "", Coef = "", stringsAsFactors = FALSE),  # Linha em branco
    data.frame(term = "R²", Coef = round(stats[[2]], 3), stringsAsFactors = FALSE),
    data.frame(term = "F-test", Coef = round(stats[[3]][[1]], 3), stringsAsFactors = FALSE),
    data.frame(term = "Prob", Coef = paste0("(",sprintf("%.2f",round(stats[[3]][[2]], 3)),")"), stringsAsFactors = FALSE),
    data.frame(term = "N", Coef = stats[[1]], stringsAsFactors = FALSE)
  )
  return(table)
}



###### Estimating 2SLS  - With Controls #############

eq1_control <- feols(map_risco_inund ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm + total_mun_affected,
                     se="hetero", data=dados2)
results1_control <- table_reg(eq1_control)

eq2_control <- feols(fisc_ench  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm + total_mun_affected,
                     se="hetero", data=dados2)
results2_control <- table_reg(eq2_control)


eq3_control <- feols(sist_alert_ench  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm + total_mun_affected,
                     se="hetero", data=dados2)
results3_control <- table_reg(eq3_control)

eq4_control <- feols(prog_hab_realoc  ~ cobertura_agua +
                       p_informal + p_urbana + log_gdp_pc + log_population + count_100mm + total_mun_affected,
                     se="hetero", data=dados2)
results4_control <- table_reg(eq4_control)

results <- cbind(results1_control,
                 results2_control[,2],
                 results3_control[,2]) %>% data.frame

names(results) <- c("","map_risco_inund","fisc_ench","sist_alert_ench")
results
write.csv2(results,"./results/tables/05_ols_controls.csv",row.names =  F)
