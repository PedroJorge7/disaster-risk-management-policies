library(dplyr)
library(fixest)
library(did)
library(forcats)

m1 <- c('main_variables','share_desalojados','map_risco_inund','fisc_ench','sist_alert_ench')
m2 <- c('control_variables','cobertura_agua','total_mun_affected','count_100mm','p_urbana','p_informal','log_gdp_pc','log_population')
m3 <- c('instrumental_variables','z_mean_map_risco_inund','z_mean_fisc_ench','z_mean_sist_alert_ench')
m4 <- c('A. Main Variables','Share Dislodged','Flood Mapping','Flood Supervision','Warning System',
        'B. Control Variables','Watershed Rate','Total disasters (2003-2019)','Count 100mm',"Urbanization Rate","Informality rate","Log(GDP Per capita)","Log(Population)",
        'C. Instrumental Variables','Mean Neighborhood Flood Mapping','Mean Neighborhood Flood Supervision','Mean Neighborhood Warning System')


dados <- readRDS("./output/data_did.rds") 
dados2 <- dados %>% filter(mun_affected == 1)


out <- dados2 %>% 
  mutate(main_variables = NA,
         control_variables = NA,
         instrumental_variables = NA) %>% 
  select(c(m1,m2,m3)) %>% 
  mutate(across(c(m1,m2,m3), ~ as.numeric(.))) %>% 
  tidyr::pivot_longer(c(m1,m2,m3),
                      names_to = 'var', values_to = 'value') %>% 
  group_by(var) %>% 
  dplyr::summarise(mean = mean(value, na.rm = T), 
                   sd   = sd(value, na.rm = T), 
                   min  = min(value, na.rm = T), 
                   max  = max(value, na.rm = T)) %>% 
  mutate(mean = ifelse(nchar(round(mean,2))==0,round(mean,4),round(mean,2)),
         sd = ifelse(nchar(round(sd,2))==0,round(sd,4),round(sd,2)),
         min = ifelse(nchar(round(min,2))==0,round(min,4),round(min,2)),
         max = ifelse(nchar(round(max,2))==0,round(max,4),round(max,2))) %>% 
  arrange(factor(var, levels = c(m1,m2,m3)))

out[,1] <- m4

write.csv2(out,"./results/tables/01_summary_statistics.csv",row.names =  F)

