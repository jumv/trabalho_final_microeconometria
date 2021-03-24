
# Importando pacotes
library(tidyverse)
library(tibbletime)
library(lubridate)

# Importando dados
dp_stats_base <- read.csv('dp_stats_base.csv')

#dp_stats_base$date <- ymd(dp_stats_base$date)
#dp_stats_base <- as_tbl_time(dp_stats_base,index = date)


# A linhas duplicadas por causas das dps que atuam em mais de 1 bairro
# Exemplo, a 14 dp atua em leblon e ipanema, logo existiam duas linhas
# com a mesma informacao que so diferenciava o nome do bairro (onde tem base da sp)
dp_stats_base  <- distinct(dp_stats_base, CISP, mes,ano,.keep_all = TRUE)

# Criando a coluna time para verificar se aquele individuo ja teve tratamento ou nao
dp_stats_base <- dp_stats_base %>% mutate(time = ifelse(ano > 2014,1,0))

# Pegando corte temporal 2010 e 2019
dp_stats_base <- dp_stats_base %>%
  filter(ano %in% c(2010,2019))

# Selecionando apenas as colunas para o estudo

df_diff_and_diff <- dp_stats_base %>%
  select(tx_letalidade,tx_roubo,tx_furtos,treatment,time,ano,mes)

# Criando coluna de diff in diff
df_diff_and_diff  <- df_diff_and_diff %>% mutate(did = treatment * time)

# Regressao com tx de roubo
diff_diff_roubos = lm(tx_roubo ~ treatment + time + did,data = df_diff_and_diff)

# Regressao com tx de furtos
diff_diff_furtos = lm(tx_furtos ~ treatment + time + did,data = df_diff_and_diff)

# Regressao com tx de letalidade
diff_diff_letalidade = lm(tx_letalidade ~ treatment + time + did,data = df_diff_and_diff)

# Resumo da regressao de roubos
summary(diff_diff_roubos)

# Resumo da regressao de roubos
summary(diff_diff_furtos)

# Resumo da regressao de roubos
summary(diff_diff_letalidade)





