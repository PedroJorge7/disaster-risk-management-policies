## dados munic -----
library(dplyr)
library(sf)
library(ggplot2)

mun <- geobr::read_municipality()
estados <- geobr::read_state()
data_did <- readRDS("./output/data_did.rds")
munic = data_did %>% 
  select(c(code_muni,map_risco_inund,fisc_ench,sist_alert_ench)) %>% distinct(code_muni, .keep_all = T) %>% 
  left_join(mun) %>% 
  st_as_sf()

# munic$adoption <- factor(ifelse(is.na(munic$map_risco_inund) | is.na(munic$fisc_ench) | is.na(munic$sist_alert_ench), 
#                                 "No data", 
#                                 ifelse(munic$map_risco_inund == 1 | munic$fisc_ench == 1 | munic$sist_alert_ench == 1, 
#                                        "Yes", "No")),
#                          levels = c("Yes", "No", "No data"))

munic$map_risco_inund <- ifelse(munic$map_risco_inund== 1,'Yes',ifelse(munic$map_risco_inund == 0,'No','No data'))
munic$fisc_ench <- ifelse(munic$fisc_ench== 1,'Yes',ifelse(munic$fisc_ench == 0,'No','No data'))
munic$sist_alert_ench <- ifelse(munic$sist_alert_ench== 1,'Yes',ifelse(munic$sist_alert_ench == 0,'No','No data'))

munic$map_risco_inund <- factor(munic$map_risco_inund, levels = c("Yes", "No", "No data"))
munic$fisc_ench <- factor(munic$fisc_ench, levels = c("Yes", "No", "No data"))
munic$sist_alert_ench <- factor(munic$sist_alert_ench, levels = c("Yes", "No", "No data"))



# Criar os gráficos individualmente com a mesma escala de cores e legenda
map_munic1 <- ggplot() +
  geom_sf(data=munic, aes(fill = map_risco_inund), color = NA) +
  labs(subtitle = "Panel A. Flood Risk Mapping") +
  geom_sf(data=estados, fill=NA, colour="black") + 
  scale_fill_manual(values=c("#2c7fb8","gray90", "white"), 
                    name = "Adoption", 
                    labels = c("Yes", "No", "No data")) + 
  theme_minimal() +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) 

map_munic2 <- ggplot() +
  geom_sf(data=munic, aes(fill = fisc_ench), color = NA) +
  labs(subtitle = "Panel B. Supervision of Risky Areas") +
  geom_sf(data=estados, fill=NA, colour="black") + 
  scale_fill_manual(values=c("#2c7fb8","gray90", "white"), 
                    name = "Adoption", 
                    labels = c("Yes", "No", "No data")) + 
  theme_minimal() +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) 

map_munic3 <- ggplot() +
  geom_sf(data=munic, aes(fill = sist_alert_ench), color = NA) +
  labs(subtitle = "Panel C. Warning System") +
  geom_sf(data=estados, fill=NA, colour="black") + 
  scale_fill_manual(values=c("#2c7fb8","gray90", "white"), 
                    name = "Adoption", 
                    labels = c("Yes", "No", "No data")) + 
  theme_minimal() +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) 

library(patchwork)
# Combinar os gráficos com uma legenda única
map_munic <- map_munic1 + map_munic2 + map_munic3 + 
  plot_layout(nrow = 2, guides = "collect") &
  theme(legend.position = "bottom") 

ggsave(map_munic,
       filename  = './results/figures/01_map_munic.png',
       dpi=300, width = 15, height = 15, units='cm')

# save plot
# ggsave(map_munic1,
#        filename  = './results/figures/00_map_munic_panel_A.png',
#        dpi=500, width = 10, height = 5, units='cm')
# 
# ggsave(map_munic2,
#        filename  = './results/figures/00_map_munic_panel_B.png',
#        dpi=500, width = 10, height = 5, units='cm')
# 
# ggsave(map_munic3,
#        filename  = './results/figures/00_map_munic_panel_C.png',
#        dpi=500, width = 10, height = 5, units='cm')
