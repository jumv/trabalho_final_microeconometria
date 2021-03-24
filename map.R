library(rgdal)
library(ggplot2)
library(sf)
library(tidyverse)

tratamento = c(9,10,12,13,14,15,16,34,32,42,21,27,29,23,18,19,20,1,4,5,6)
controle =  c(7,17,22,24,25,26,28,30,31,33,35,36,37,38,39,40,41,43,44)

shp <- st_read("shapefile/lm_dp_2019.shp")

shp_capital <- shp %>% filter(dp %in% c(controle,tratamento))
shp_capital <- shp_capital %>% mutate(treatment = case_when(dp %in% tratamento ~ 'Tratamento'))
shp_capital <- shp_capital %>% replace_na(list(treatment = 'Controle'))                                      


ggplot(data = shp_capital) +
  geom_sf(aes(fill = treatment))
