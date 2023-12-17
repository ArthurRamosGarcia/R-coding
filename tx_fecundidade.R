


############################################################
###########################################
############################

# IMPORTACAO DOS DADOS

tx_fecundidade = ipeadatar::ipeadata(code = 'DEPIS_TFEC')

pop_urbana = ipeadatar::ipeadata(code = 'DEPIS_POPURB')

# TRATAMENTO DOS DADOS

dados = dplyr::inner_join(tx_fecundidade, pop_urbana, by='date')%>%
  dplyr::select(date, value.x, value.y)%>%
  dplyr::rename(tx_fecundidade=value.x, pop_urbana=value.y)


# VISUALIZACAO DOS DADOS

g1=dados%>%
  dplyr::mutate(pop_urb_indice = pop_urbana / pop_urbana[1],
                tx_fecundidade_indice = tx_fecundidade / tx_fecundidade[1])%>%
  ggplot(aes(x=date))+
  geom_line(mapping=aes(y=pop_urb_indice, colour='População Urbana'))+
  geom_line(mapping=aes(y=tx_fecundidade_indice, colour='Taxa de Fecundiade'))+
  labs(x='',y='',title="População Urbana vs Taxa de Fecundidade")+
  scale_x_date(breaks = date_breaks("1 year"), labels = date_format("%Y"))+
  scale_y_continuous(breaks = seq(0.6,1.2,0.05))+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45, hjust=1))
  
# TESTE DE ESTACIONARIEDADE

pop_urbana_ts = dados%>%
  dplyr::select(pop_urbana)%>%
  as.ts()

tx_fecundidade_ts = dados%>%
  dplyr::select(tx_fecundidade)%>%
  as.ts()


adf.test(pop_urbana_ts, c('stationary', 'explosive'))
adf.test(tx_fecundidade_ts, c('stationary', 'explosive'))

# CRIACAO DO MCE

dados=dados%>%
  dplyr::mutate(def_1_tx_fecundidade = dplyr::lag(tx_fecundidade, 1))%>%
  tidyr::drop_na()

# MODELAGEM

summary(stats::lm(formula = tx_fecundidade ~ def_1_tx_fecundidade + pop_urbana, data = dados))



