
# Importando pacotes
library(tidyverse)
library(tibbletime)
library(lubridate)
library(plm)

tratamento = c(9,10,12,13,14,15,16,34,32,42,21,27,29,23,18,19,20,1,4,5,6)
controle =  c(7,17,22,24,25,26,28,30,31,33,35,36,37,38,39,40,41,43,44)

# Tratamento para diff in diff
collect_data <- function(y,ano_corte,ano_zero,ano_um,dp_controle = controle,
                      dp_tratamento = tratamento){
  # Importando dados
  dp_stats_base <- read.csv('dp_stats_base.csv')
  
  # A linhas duplicadas por causas das dps que atuam em mais de 1 bairro
  # Exemplo, a 14 dp atua em leblon e ipanema, logo existiam duas linhas
  # com a mesma informacao que so diferenciava o nome do bairro (onde tem base da sp)
  dp_stats_base  <- distinct(dp_stats_base, CISP, mes,ano,.keep_all = TRUE)
  
  # Criando a coluna time para verificar se aquele individuo ja teve tratamento ou nao
  dp_stats_base <- dp_stats_base %>% mutate(time = ifelse(ano > ano_corte,1,0))
  
  # Pegando corte temporal e as delegacias de controle e tratamento
  dp_stats_base <- dp_stats_base %>%
    filter(ano %in% c(ano_zero,ano_um),CISP %in% c(dp_controle,dp_tratamento))
  
  # Definindo quem e tratamento e quem e controle
  dp_stats_base <- dp_stats_base %>% mutate(treatment = case_when(CISP %in% dp_tratamento ~ 1,
                                                                  !CISP %in% dp_tratamento ~ 0))
  # Selecionando apenas as colunas para o estudo
  df_diff_and_diff <- dp_stats_base %>%
    select(.data[[y]],treatment,time,date,CISP)
  # return 
  df_diff_and_diff
}

# # diff in diff  
# diff_in_diff <- function(ano_corte,ano_zero,ano_um,dp_controle = controle,
#                         dp_tratamento = tratamento){
#   # Tratando dados
#   df_diff_and_diff = collect_data(ano_corte,ano_zero,ano_um,dp_controle = controle,
#                                   dp_tratamento = tratamento)
#   # Criando coluna de diff in diff
#   df_diff_and_diff  <- df_diff_and_diff %>% mutate(did = treatment * time)
#   
#   # Regressao com tx de roubo
#   diff_diff_roubos = lm(tx_roubo ~ treatment + time + did,data = df_diff_and_diff)
#   
#   # Regressao com tx de furtos
#   diff_diff_furtos = lm(tx_furtos ~ treatment + time + did,data = df_diff_and_diff)
#   
#   # Regressao com tx de letalidade
#   diff_diff_letalidade = lm(tx_letalidade ~ treatment + time + did,data = df_diff_and_diff)
#   
#   list(diff_diff_furtos,diff_diff_roubos,diff_diff_letalidade)
#   
# }  
  
# fixed effects

diff_in_diff <- function(y,ano_corte,ano_zero,ano_um,dp_controle = controle,
                          dp_tratamento = tratamento,effect = NULL){
  # Tratando dados
  df_diff_and_diff = collect_data(y,ano_corte,ano_zero,ano_um,dp_controle = controle,
                                  dp_tratamento = tratamento)
  # Criando coluna de diff in diff
  df_diff_and_diff  <- df_diff_and_diff %>% mutate(did = treatment * time)
  
  # Criando regressao
  f <- paste(y, "~", paste('treatment','time','did', sep =" + "))
  if (is.null(effect)){
    plm(f,
        data = df_diff_and_diff,
        index = c("CISP", "date"), 
        model = "within")
  }
  else{
    plm(f,
        data = df_diff_and_diff,
        index = c("CISP", "date"), 
        model = "within",
        effect = 'twoways')
  }
  
}
  
  # # Regressao com efeito de entidade fixa furtos
  # entity_fixed_furtos <- plm(tx_furtos ~ treatment + time + did, 
  #                     data = df_diff_and_diff,
  #                     index = c("CISP", "date"), 
  #                     model = "within")
  # # Regressao com efeito de entidade fixa furtos
  # time_fixed_furtos <- plm(tx_furtos ~ treatment + time + did, 
  #                            data = df_diff_and_diff,
  #                            index = c("CISP", "date"), 
  #                            model = "within",
  #                            effect = 'twoways')
  # # Regressao com efeito de entidade fixa roubos
  # entity_fixed_roubos <- plm(tx_roubo ~ treatment + time + did, 
  #                            data = df_diff_and_diff,
  #                            index = c("CISP", "date"), 
  #                            model = "within")
  # # Regressao com efeito de entidade fixa furtos
  # time_fixed_roubos <- plm(tx_roubo ~ treatment + time + did, 
  #                          data = df_diff_and_diff,
  #                          index = c("CISP", "date"), 
  #                          model = "within",
  #                          effect = 'twoways')
  # # Regressao com efeito de entidade fixa furtos
  # entity_fixed_letalidade <- plm(tx_letalidade ~ treatment + time + did, 
  #                            data = df_diff_and_diff,
  #                            index = c("CISP", "date"), 
  #                            model = "within")
  # # Regressao com efeito de entidade fixa furtos
  # time_fixed_letalidade <- plm(tx_letalidade ~ treatment + time + did, 
  #                          data = df_diff_and_diff,
  #                          index = c("CISP", "date"), 
  #                          model = "within",
  #                          effect = 'twoways')
  # 
  # list(entity_fixed_furtos,time_fixed_furtos,entity_fixed_roubos,time_fixed_roubos,
  #      entity_fixed_letalidade,time_fixed_letalidade)
  

  
  














