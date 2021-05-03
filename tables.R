
## Pacotes
library(tidyverse)
library(stargazer)


# Importando dados
df_pop <- read_csv2('PopulacaoEvolucaoMensalCisp.csv')

# DPs
tratamento = c(9,10,12,13,14,15,16,34,32,42,21,27,29,23,18,19,20,1,4,5,6)
controle =  c(7,17,22,24,25,26,28,30,31,33,35,36,37,38,39,40,41,43,44)


# Filtrando Tabela

df_pop %>% filter(cisp %in% c(tratamento,controle),
                  ano == c(2010,2019),
                  mes == 12) %>%
  mutate(grupo = ifelse(cisp %in% tratamento,'Tratamento','Controle')) %>%
  group_by(ano,grupo) %>% summarise(media = round(mean(pop),0),
                                    desv_pad = round(sd(pop),0),
                                    max = round(max(pop),0),
                                    min = round(min(pop),0)) %>% 
  stargazer(summary = F,type = 'text')

## Por assaltos

df_stats <- read_csv2('BaseDPEvolucaoMensalCisp.csv') 

pop <- df_pop %>% filter(cisp %in% c(tratamento,controle),
                  ano == c(2010,2019),
                  mes == 12) %>%
  mutate(grupo = ifelse(cisp %in% tratamento,'Tratamento','Controle')) %>%
  group_by(ano,grupo) %>% summarise(total = sum(pop))


df_stats %>% filter(CISP %in% c(tratamento,controle),
                  ano == c(2010,2019)) %>%
  mutate(grupo = ifelse(CISP %in% tratamento,'Tratamento','Controle')) %>%
  group_by(ano,grupo) %>% summarise(total_roubos = round(sum(total_roubos),0),
                                    total_furtos = round(sum(total_furtos),0),
                                    total_letalidade = round(sum(letalidade_violenta),0)) %>% left_join(pop ,
                                                                                                        by = c('ano','grupo')) %>%
  mutate(total_roubos = round((total_roubos/total) * 10000),
         total_furtos = round((total_furtos/total) * 10000),
         total_letalidade = round((total_letalidade/total) * 10000)) %>%
  select(-total) %>% stargazer(summary = F,type = 'text') 



## 

df_pop %>% filter(cisp %in% c(tratamento,controle),
                  ano == c(2010,2019),
                  mes == 12) %>%
  group_by(cisp) %>%
  summarise(populacao = as.integer(mean(pop))) %>%
  left_join(read_csv('dps_bairros.csv') %>%
              select(cisp = DP,`Unidade Territorial`),by = 'cisp') %>%
  select(cisp,populacao_media = populacao,`Unidade Territorial`) %>% arrange(desc(populacao_media)) %>%
  stargazer(summary = F,type = 'html')


  
  


