source('regressions.R')


# Sem efeitos fixos

diff_furtos <- diff_in_diff('tx_furtos',2014,2010,2019)
diff_roubos <- diff_in_diff('tx_roubo',2014,2010,2019)
diff_letalidade <- diff_in_diff('tx_letalidade',2014,2010,2019)
diff_desaparecidos <- diff_in_diff('tx_pessoas_desaparecidas',2014,2010,2019)

# Com efeitos Fixos

diff_furtos_fixed <- diff_in_diff('tx_furtos',2014,2010,2019,effect = 'twoways')
diff_roubos_fixed <- diff_in_diff('tx_roubo',2014,2010,2019,effect = 'twoways')
diff_letalidade_fixed <- diff_in_diff('tx_letalidade',2014,2010,2019,effect = 'twoways')
diff_desaparecidos_fixed <- diff_in_diff('tx_pessoas_desaparecidas',2014,2010,2019,effect = 'twoways')


stargazer::stargazer(diff_furtos,diff_roubos,diff_letalidade,diff_desaparecidos, align=TRUE,type = 'text')

stargazer::stargazer(diff_furtos_fixed,diff_roubos_fixed,diff_letalidade_fixed,diff_desaparecidos_fixed, align=TRUE,type = 'text')


