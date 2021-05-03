
# Import packages
library(tidyverse)
library(lubridate)

# Importando dados
bases_seg_presente <- read.csv('bases_seguranca_presente.csv')[,-1]
df_dps_bairros <- read.csv('dps_bairros.csv')[,-1]

# Trocando a conjuncao 'e' por virgula na separacao dos bairros
df_dps_bairros$Unidade.Territorial <- df_dps_bairros$Unidade.Territorial %>%
  str_replace(' e',',')

# Colocando cada bairro em uma linha separada 
df_dps_bairros <- df_dps_bairros %>% separate_rows(Unidade.Territorial,sep = ', ')
bases_seg_presente  <- bases_seg_presente %>% separate_rows(BASE,sep = '/')

# Selecionando apenas as colunas necessarias de cada base de dados
df_dps_bairros <- df_dps_bairros %>% select(DP,Unidade.Territorial)
bases_seg_presente <- bases_seg_presente %>% select(BASE,INAUGURAÇÃO)

# OBS

# O Centro e servido por 4 dps e tambem por 5 bases do seguranca presente
# E como as bases foram colocadas em periodos diferentes
# Precisamos tentar relacionar cada base com a area de atuacao da DP


# A DP 1 fica localizada na praca maua, logo deve supor que a base do
# do seguranca presente da praca maua seja de area de influencia da mesma

# A DP 4 atua na regiao portuaria (gamboa,saude,santo cristo) 
# Logo deve relacionar com a base que atua na presidente vargas

# A DP 5 atua na LAPA, paqueta e parte do centro, logo as bases da
# carioca e praca xv devem ser da mesma

# A DP 6 atua no centro perto do sambodramo(catumbi,rio comprido,praca 11) 
# logo deve se relacionar com a base da cruz vermelha

# Mudando nos dados para fazer um join

# Praca xv e carioca (delegacia 5)
bases_seg_presente[20:21,1] <- 'Centro1'
bases_seg_presente[20:21,2] <- '2016-07-26' 
df_dps_bairros[48,2] <- 'Centro1'

# Praca maua (delegacia 1)
bases_seg_presente[22,1] <- 'Centro2'
df_dps_bairros[43,2] <- 'Centro2'

# Presidente Vargas (delegacia 4)
bases_seg_presente[23,1] <- 'Centro3'
df_dps_bairros[44,2] <- 'Centro3'

# Cruz vermelha (delegacia 6)
bases_seg_presente[24,1] <- 'Centro4'
df_dps_bairros[38,2] <- 'Centro4'

# Retirando duplicata da base  Praca xv e carioca
bases_seg_presente <- bases_seg_presente[-20,]

# Relacionando os bairros com seguranca presente para area de influencia da delegacia
df_dps_bases <- bases_seg_presente %>% left_join(df_dps_bairros,by = c('BASE' = 'Unidade.Territorial'))

# Tirando o cristo redentor
df_dps_bases <- df_dps_bases[-5,]

# Criando coluna para indicar que essas dps sao do grupo de tratamento
df_dps_bases <- df_dps_bases %>% mutate(treatment = 1) 

# Importando estatisticas de crime por delegacia
dp_stats <- read.csv('BaseDPEvolucaoMensalCisp.csv',sep = ';',encoding = 'latin1')

# Retirando espacos brancos
dp_stats$Regiao <- dp_stats$Regiao %>% str_trim()


# Selecionando apenas crimes de interesse na capital
dp_stats <- dp_stats %>% filter(Regiao == 'Capital') %>%
  select(CISP,mes,ano,letalidade_violenta,total_roubos,total_furtos,pessoas_desaparecidas)
  

# Juntando as delegacias do grupo de tratamento e as estatisticas de todas as delegacias
dp_stats_base <- dp_stats %>% left_join(df_dps_bases,by = c('CISP' = 'DP'))

# Preenchendo a coluna treatment com zero para as delegacias sem tratamento
# Preenchendo as datas do grupo de controle com 2020-01-01
# Ja que iremos ate 2019-31-12
dp_stats_base <- dp_stats_base %>% 
  replace_na(list(treatment = 0, INAUGURAÇÃO = "2020-01-01"))

# Criando coluna de data no formato YYYY-MM-DD para podermos verificar quando
# comecou o programa para cada individuo de tratamento
dp_stats_base <- dp_stats_base %>% mutate(date=ymd(paste(ano,mes,'01', sep = '-')))

# Criando a coluna time para verificar se aquele individuo ja teve tratamento ou nao
#dp_stats_base <- dp_stats_base %>% mutate(time = ifelse(ano > 2014,1,0))

# Filtrando o intervalo de tempo entre 2010 e 2019
#dp_stats_base <- dp_stats_base  %>% filter(ano %in% c(2010,2019))

# Importando base de populacao por dp
df_pop <- read.csv('PopulacaoEvolucaoMensalCisp.csv',sep = ';')

# Juntando dados de pop para nossa base de estatisticas
dp_stats_base <- dp_stats_base %>% left_join(df_pop, by = c('CISP' = 'cisp' ,'mes' = 'mes','ano' = 'ano'))

# A coluna de pop nao veio numerica, estamos transformando - na
dp_stats_base <- dp_stats_base %>% separate(pop,c('pop','trash')) %>% select(-trash)
dp_stats_base$pop <- as.numeric(dp_stats_base$pop)


# Criando as colunas de taxas (letalidade,roubo e furto) por 100 mil hab
dp_stats_base <- dp_stats_base %>% mutate(tx_letalidade = (letalidade_violenta/pop) * 100000,
                       tx_roubo = (total_roubos/pop) * 100000,
                       tx_furtos = (total_furtos/pop) * 100000,
                       tx_pessoas_desaparecidas = (pessoas_desaparecidas/pop) * 100000)

write.csv(dp_stats_base,'dp_stats_base.csv')








