#Comentário: Baixando os Pacotes Necessários
#install.packages("tidyverse")
#install.packages("xlsx")
#OBS: coloque hashtag para virar comentário e eu não precisar rodar mais

#Importar pacotes
library(tidyverse)
library(xlsx)


#Importar dados da tabela IDH

df_idh = read.xlsx("IDH_1991.xls",sheetIndex = 2,startRow = 6,header = T)


#Separando o nomes dos bairros em linhas

df_idh_sep = df_idh %>% separate_rows(Bairro.ou.grupo.de.bairros,sep = ', ')

#Importar dados da tabela de pop_residence_domicilio

df_pop <- read.xlsx("Pop_residente_domicilio.xls",sheetIndex = 1,startRow = 4,header = T)

# Retirando as linhas inúteis (as 1as 2 linhas)

df_pop = df_pop %>% slice(3:n())

#Retirando colunas inúteis da tabela de IDH

df_idh_sep = df_idh_sep %>% select(Bairro.ou.grupo.de.bairros,Índice.de.Desenvolvimento.Humano.Municipal..IDH.)

#Renomeando Coluna

df_idh_sep = df_idh_sep %>% rename(Bairro=Bairro.ou.grupo.de.bairros,
                                   IDH = Índice.de.Desenvolvimento.Humano.Municipal..IDH.)

#Retirando os NA

df_idh_sep = df_idh_sep %>% drop_na()

df_pop[67,1] <- "São Cristóvão"
df_pop[105,1] <- "Parque Colúmbia"
df_pop[52,1] <- "Freguesia"
df_pop[158,1] <- "Vila Cosmos"


#Unindo tabelas pq falo português (Join)
df_idh_pop = df_idh_sep %>% left_join(df_pop,by=c("Bairro"="Bairros"))

# Replicando tabela 


