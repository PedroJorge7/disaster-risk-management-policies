#### Tabela 1 – Taxa de Adoção das Políticas no Brasil e nas Macrorregiões (% de municípios que adotam). ------------
library(dplyr)
data_did <- readRDS("./output/data_did.rds")

reg <- data_did %>%
  distinct(code_muni, .keep_all = T) %>% 
  mutate(reg = substr(code_muni,1,1)) %>%
  group_by(reg) %>%
  dplyr::summarise(sist_alert_ench = mean(sist_alert_ench,na.rm= T),
                   fisc_ench       = mean(fisc_ench,na.rm= T),
                   map_risco_inund = mean(map_risco_inund,na.rm= T)
                   #adota_regulamentacao = sum(adota_regulamentacao,na.rm= T)
  ) %>% 
  mutate(reg = ifelse(reg == 1,'North region',
                      ifelse(reg == 2,'Northeast region',
                             ifelse(reg == 3,'Southeast region',
                                    ifelse(reg == 4,'South region',
                                           ifelse(reg == 5,'Midwest region',NA))))))
br <- data_did %>%
  distinct(code_muni, .keep_all = T) %>% 
  dplyr::summarise(sist_alert_ench = mean(sist_alert_ench,na.rm= T),
                   fisc_ench = mean(fisc_ench,na.rm= T),
                   map_risco_inund = mean(map_risco_inund,na.rm= T)
                   #adota_regulamentacao = sum(adota_regulamentacao,na.rm= T)
  )
br$reg <- 'Brazil'
tabela <- bind_rows(reg,br)
tabela_taxa <- tabela %>% 
  mutate(sist_alert_ench = (sist_alert_ench)*100,
         fisc_ench = (fisc_ench)*100,
         map_risco_inund = (map_risco_inund)*100)

write.csv2(tabela_taxa,"./results/tables/00_adocao_das_politicas.csv", row.names = F)
