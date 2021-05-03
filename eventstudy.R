
library(tidyverse)
library(tibbletime)
library(lubridate)
library(plm)
library(eventStudy)
library(data.table)
library(ggplot2)

source('regressions.R')


tratamento = c(9,10,12,13,14,15,16,34,32,42,21,27,29,23,18,19,20,1,4,5,6)
controle =  c(7,17,22,24,25,26,28,30,31,33,35,36,37,38,39,40,41,43,44)

# Importando dados
dp_stats_base <- read.csv('dp_stats_base.csv')

# A linhas duplicadas por causas das dps que atuam em mais de 1 bairro
# Exemplo, a 14 dp atua em leblon e ipanema, logo existiam duas linhas
# com a mesma informacao que so diferenciava o nome do bairro (onde tem base da sp)
dp_stats_base  <- distinct(dp_stats_base, CISP, mes,ano,.keep_all = TRUE)

# Criando a coluna time para verificar se aquele individuo ja teve tratamento ou nao
dp_stats_base <- dp_stats_base %>% mutate(time = ifelse(ano > 2014,1,0))

# Pegando corte temporal e as delegacias de controle e tratamento
dp_stats_base <- dp_stats_base %>%
  filter(CISP %in% c(controle,tratamento))

# Definindo quem e tratamento e quem e controle
dp_stats_base <- dp_stats_base %>% mutate(treatment = case_when(CISP %in% tratamento ~ 1,
                                                                !CISP %in% tratamento ~ 0))

# Formatando dados

dp_stats_base <- dp_stats_base %>% mutate(ano_tratamento = year(INAUGURAÇÃO))

df <- dp_stats_base %>% filter(ano %in% 2010:2019) %>% select(tx_letalidade,CISP,ano,ano_tratamento)  
  

# df$tx_furtos <- as.character(df$tx_furtos)
df$CISP <- as.character(df$CISP)
df$date <- as.integer(df$ano)
df$INAUGURAÇÃO <- as.integer(df$ano_tratamento)




dt <- as.data.table(df)


results <- ES(long_data=dt, outcomevar="tx_letalidade", 
              unit_var="CISP", cal_time_var="date", 
              onset_time_var="INAUGURAÇÃO", cluster_vars="CISP",omitted_event_time = -1)


ES_plot_ATTs(results, lower_event = -5, upper_event = 5, homogeneous_ATT = TRUE,omitted_event_time = -1) + 
  ylab("ATT Estimate (95% CI)") +
  labs(title = 'Event Study para taxa de letalidade')



