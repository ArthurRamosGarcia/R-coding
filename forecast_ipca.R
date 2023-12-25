
library(readlxl)
library(dplyr)
library()tidyr)
library(stats)
library(lubridate)
library(forecast)
library(tseries)

ipca = GetBCBData::gbcbd_get_series(id=433, use.memoise = F)%>%
  dplyr::select(ref.date, value)%>%
  dplyr::rename(ipca=value, data=ref.date)


desemprego = sidrar::get_sidra(api='/t/6381/n1/all/v/4099/p/all/d/v4099%201')%>%
  dplyr::select(Valor, `Trimestre Móvel (Código)`)


writexl::write_xlsx(desemprego, 'C:/Users/Arthur/Desktop/Faculdade + Cursos/Programação/desemprego.xlsx')

desemprego=readxl::read_xlsx('C:/Users/Arthur/Desktop/Faculdade + Cursos/Programação/desemprego.xlsx')

desemprego$data=as.Date(desemprego$data)

tx_selic=GetBCBData::gbcbd_get_series(id=11, use.memoise = F)%>%
  dplyr::select(ref.date, value)%>%
  dplyr::rename(selic=value, data=ref.date)

tx_cambio=GetBCBData::gbcbd_get_series(id=1, use.memoise = F)%>%
  dplyr::select(ref.date, value)%>%
  dplyr::rename(cambio=value, data=ref.date)

dados=dplyr::inner_join(ipca, desemprego, by='data')

lubridate::day(dados$data) = ifelse(month(dados$data)==01 | month(dados$data)==05 | month(dados$data)==12, 08, 01)

dados=dplyr::inner_join(dados,tx_selic, by='data')
dados=dplyr::inner_join(dados,tx_cambio,by='data')%>%
  dplyr::mutate(dummy_jan = ifelse(month(as.Date(dados$data)) == 01, 1, 0),
                dummy_fev = ifelse(month(as.Date(dados$data)) == 02, 1, 0),
                dummy_mar = ifelse(month(as.Date(dados$data)) == 03, 1, 0),
                dummy_abr = ifelse(month(as.Date(dados$data)) == 04, 1, 0),
                dummy_mai = ifelse(month(as.Date(dados$data)) == 05, 1, 0),
                dummy_jun = ifelse(month(as.Date(dados$data)) == 06, 1, 0),
                dummy_jul = ifelse(month(as.Date(dados$data)) == 07, 1, 0),
                dummy_ago = ifelse(month(as.Date(dados$data)) == 08, 1, 0),
                dummy_set = ifelse(month(as.Date(dados$data)) == 09, 1, 0),
                dummy_out = ifelse(month(as.Date(dados$data)) == 10, 1, 0),
                dummy_nov = ifelse(month(as.Date(dados$data)) == 11, 1, 0),
                dummy_dez = ifelse(month(as.Date(dados$data)) == 12, 1, 0))


modelo = stats::lm(formula = 'ipca~dummy_jan+dummy_fev+
                   dummy_mar+dummy_abr+dummy_mai+dummy_jun+
                   dummy_jul+dummy_ago+dummy_set+dummy_out+
                   dummy_nov+dummy_dez', data=dados)

modelo$coefficients[1]

x=dados%>%
  dplyr::mutate(ipca_dessaz = ifelse(month(dados$data)==01,ipca*(1+modelo$coefficients[2]),
                                     ifelse(month(dados$data)==02, ipca*(1+modelo$coefficients[3]),
                                            ifelse(month(dados$data)==03, ipca*(1+modelo$coefficients[4]),
                                                   ifelse(month(dados$data)==04, ipca*(1+modelo$coefficients[5]),
                                                          ifelse(month(dados$data)==05, ipca*(1+modelo$coefficients[6]),
                                                                 ifelse(month(dados$data)==06, ipca*(1+modelo$coefficients[7]),
                                                                        ifelse(month(dados$data)==07, ipca*(1+modelo$coefficients[8]),
                                                                               ifelse(month(dados$data)==09, ipca*(1+modelo$coefficients[10]),
                                                                                      ifelse(month(dados$data)==10, ipca*(1+modelo$coefficients[11]),
                                                                                             ifelse(month(dados$data)==11, ipca*(1+modelo$coefficients[12]),
                                                                                                    ipca)))))))))))%>%
  dplyr::mutate(ipca_i = ipca_dessaz / ipca_dessaz[1],
                ipca_i_2 = ipca / ipca[1],
                desemprego_i = desemprego / desemprego[1],
                selic_i = selic / selic[1],
                cambio_i = cambio / cambio[1])



x%>%
  ggplot(aes(x=data))+
  geom_line(mapping=aes(y=ipca_dessaz, colour='IPCA dessaz'))+
  geom_line(mapping=aes(y=ipca, colour='IPCA'))+
  geom_line(mapping=aes(y=desemprego_i, colour='Taxa de Desocupação'))+
  geom_line(mapping=aes(y=selic_i, colour='Taxa Selic'))+
  geom_line(mapping=aes(y=cambio_i, colour='Taxa de Câmbio - Venda'))+
  theme_classic()+
  scale_x_date(breaks=date_breaks('3 months'))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title = 'Evolução Macro no Brasil', susbtitle='Valores em número-índice',
       x='', y ='')+
  scale_y_continuous(breaks=seq(-1,2.5,0.2))


ipca_ts = dados%>%
  dplyr::select(ipca)%>%
  as.ts()

desemprego_ts = dados%>%
  dplyr::select(desemprego)%>%
  as.ts()

selic_ts = dados%>%
  dplyr::select(selic)%>%
  as.ts()

cambio_ts = dados%>%
  dplyr::select(cambio)%>%
  as.ts()


adf.test(ipca_ts, c('stationary', 'explosive'))
adf.test(desemprego_ts, c('stationary', 'explosive'))
adf.test(selic_ts, c('stationary', 'explosive'))
adf.test(cambio_ts, c('stationary', 'explosive'))


modelo_2 = stats::lm(formula = 'ipca~dummy_jan+dummy_fev+dummy_mar+dummy_abr+dummy_mai+dummy_jun++dummy_jul+dummy_ago+dummy_set+dummy_out+dummy_nov+desemprego+selic+cambio', data = dados)  

summary(modelo_2)

plot(residuals(modelo_2))

adf.test(residuals(modelo_2), c('stationary', 'explosive'))

dados=dados%>%
  dplyr::mutate(res = residuals(modelo_2))

dados=dados%>%
  dplyr::mutate(res_def=dplyr::lag(res, 1))%>%
  tidyr::drop_na()

modelo_3 = stats::lm(formula = 'ipca~dummy_jan+dummy_fev+dummy_mar+dummy_abr+dummy_mai+dummy_jun++dummy_jul+dummy_ago+dummy_set+dummy_out+dummy_nov+desemprego+selic+cambio+res_def', data = as.ts(dados))  

adf.test(residuals(modelo_3), c('stationary', 'explosive'))

plot(residuals(modelo_3))

dados_ajustados = stats::predict(modelo_3)

dados=dados%>%
  cbind(dados_ajustados)

previsoes = forecast::forecast(modelo_3, dados, h = 6)

data_proxima = dados[nrow(dados), 'data'] + 30

as.Date(year(data_proxima), month(data_proxima)+1, day(data_proxima))


data_proxima_1 = dados[nrow(dados), 'data'] + 30
data_proxima_2 = dados[nrow(dados), 'data'] + 60
data_proxima_3 = dados[nrow(dados), 'data'] + 90
data_proxima_4 = dados[nrow(dados), 'data'] + 120
data_proxima_5 = dados[nrow(dados), 'data'] + 150
data_proxima_6 = dados[nrow(dados), 'data'] + 180

data = as.data.frame(c(data_proxima_1,data_proxima_2,data_proxima_3,data_proxima_4,data_proxima_5,data_proxima_6))

dados_prox = cbind(data, as.data.frame(previsoes))

dados_prox = as.data.frame(dados_prox)

dados_1 = dados%>%
  dplyr::select(data)

data=data%>%
  dplyr::rename(data=`c(data_proxima_1, data_proxima_2, data_proxima_3, data_proxima_4, data_proxima_5, data_proxima_6)`)


dados_1=dados_1%>%
  rbind(data)

dados_1=dados_1%>%
  cbind(previsoes)

model_desemp = forecast::auto.arima(desemprego_ts, 3)

prev_desemp = forecast::forecast(model_desemp, h=6)%>%
  as.data.frame()%>%
  dplyr::select(`Point Forecast`)%>%
  dplyr::rename(desemprego=`Point Forecast`)
  
model_selic = forecast::auto.arima(selic_ts, 3)

prev_selic = forecast::forecast(model_selic, h=6)%>%
  as.data.frame()%>%
  dplyr::select(`Point Forecast`)%>%
  dplyr::rename(selic=`Point Forecast`)

model_cambio = forecast::auto.arima(cambio_ts, 3)

prev_cambio = forecast::forecast(model_cambio, h=6)%>%
  as.data.frame()%>%
  dplyr::select(`Point Forecast`)%>%
  dplyr::rename(cambio=`Point Forecast`)


dados_novos = cbind(data, prev_desemp, prev_selic, prev_cambio)

dados_novos = dados_novos%>%
  dplyr::mutate(dummy_jan = ifelse(month(as.Date(dados_novos$data)) == 01, 1, 0),
                dummy_fev = ifelse(month(as.Date(dados_novos$data)) == 02, 1, 0),
                dummy_mar = ifelse(month(as.Date(dados_novos$data)) == 03, 1, 0),
                dummy_abr = ifelse(month(as.Date(dados_novos$data)) == 04, 1, 0),
                dummy_mai = ifelse(month(as.Date(dados_novos$data)) == 05, 1, 0),
                dummy_jun = ifelse(month(as.Date(dados_novos$data)) == 06, 1, 0),
                dummy_jul = ifelse(month(as.Date(dados_novos$data)) == 07, 1, 0),
                dummy_ago = ifelse(month(as.Date(dados_novos$data)) == 08, 1, 0),
                dummy_set = ifelse(month(as.Date(dados_novos$data)) == 09, 1, 0),
                dummy_out = ifelse(month(as.Date(dados_novos$data)) == 10, 1, 0),
                dummy_nov = ifelse(month(as.Date(dados_novos$data)) == 11, 1, 0),
                dummy_dez = ifelse(month(as.Date(dados_novos$data)) == 12, 1, 0))


previsoes_fora=forecast::forecast(modelo_2,dados_novos)%>%
  as.data.frame()%>%
  dplyr::select(`Point Forecast`)

  
previsoes_fora=previsoes_fora%>%
  dplyr::rename(ipca = "Point Forecast")


dados_novos=dados_novos%>%
  cbind(previsoes_fora)

modelo_4 = stats::lm(formula = 'ipca~dummy_jan+dummy_fev+dummy_mar+dummy_abr+dummy_mai+dummy_jun++dummy_jul+dummy_ago+dummy_set+dummy_out+dummy_nov+desemprego+selic+cambio', data = dados_novos)  

dados_novos=dados_novos%>%
  dplyr::mutate(res=residuals(modelo_4))

dados_novos=dados_novos%>%
  dplyr::mutate(res_def=dplyr::lag(res,1))%>%
  tidyr::drop_na()


dados=dados%>%
  rbind(dados_novos)

summary(modelo_4)


dados_x=dados%>%
  dplyr::select(data, ipca)

dados_novos_x=dados_novos%>%
  dplyr::select(data, ipca)


dados_final = rbind(dados_x,dados_novos_x)

seg_1 = dados_final%>%
  dplyr::filter(dados_final$data >= '2023-09-01')

seg_2=dados_final%>%
  dplyr::filter(dados_final$data < '2023-10-31')
  


ggplot(dados_final, aes(x=data))+
  geom_line(data = x, aes(y=ipca_dessaz, colour = 'IPCA Dessazonalizado'))+
  geom_line(data = seg_1, aes(y = ipca, colour='Projeção IPCA'), size=0.8)+
  geom_line(data= seg_2, aes(y = ipca, colour='IPCA'))+
  theme_classic()+
  scale_x_date(breaks = date_breaks("6 months"), labels=date_format("%b/%Y"))+
  theme(axis.text.x = element_text(angle=45, hjust=1))+
  labs(title = "IPCA Histórico e Projeção IPCA", x='', y ='')+
  scale_y_continuous(breaks=seq(-0.75,1.5,0.1))
  
