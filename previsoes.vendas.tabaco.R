
library(readxl)
library(tidyverse)
library(stats)
library(scales)
library(lubridate)
library(zoo)
library(graphics)

gera.previsao.vendas.tabaco = function(){
  
  vendas_tabaco = readxl::read_xlsx('C:/Users/Arthur/Desktop/tabaco.xlsx')%>%
    as.data.frame()
  
  vendas_tabaco$Data = as.Date(vendas_tabaco$Data)
  
  vendas_tabaco = vendas_tabaco%>%
    dplyr::mutate(vendas_tabaco = `Vendas de Tabaco`)%>%
    dplyr::mutate(vendas_tabaco_lag1 = dplyr::lag(vendas_tabaco, 1), vendas_tabaco_lag2 = dplyr::lag(vendas_tabaco, 2), vendas_tabaco_lag3 = dplyr::lag(vendas_tabaco, 3))%>%
    dplyr::select(Data, vendas_tabaco, vendas_tabaco_lag1, vendas_tabaco_lag2, vendas_tabaco_lag3)%>%
    tidyr::drop_na()
  
  modelo = stats::lm(vendas_tabaco~vendas_tabaco_lag1+vendas_tabaco_lag2+vendas_tabaco_lag3, data = vendas_tabaco)
  
  estimativa_semana_seguinte = coef(modelo)[1]+coef(modelo)[2]*vendas_tabaco[nrow(vendas_tabaco), "vendas_tabaco"]+coef(modelo)[3]*vendas_tabaco[nrow(vendas_tabaco), "vendas_tabaco_lag1"]+coef(modelo)[4]*vendas_tabaco[nrow(vendas_tabaco), "vendas_tabaco_lag2"]
  estimativa_duassemanas_depois = coef(modelo)[1]+coef(modelo)[2]*estimativa_semana_seguinte+coef(modelo)[3]*vendas_tabaco[nrow(vendas_tabaco), "vendas_tabaco"]+coef(modelo)[4]*vendas_tabaco[nrow(vendas_tabaco), "vendas_tabaco_lag1"]
  estimativa_tressemanas_depois = coef(modelo)[1]+coef(modelo)[2]*estimativa_duassemanas_depois+coef(modelo)[3]*estimativa_semana_seguinte+coef(modelo)[4]*vendas_tabaco[nrow(vendas_tabaco), "vendas_tabaco"]
  estimativa_quatrosemanas_depois = coef(modelo)[1]+coef(modelo)[2]*estimativa_tressemanas_depois+coef(modelo)[3]*estimativa_duassemanas_depois+coef(modelo)[4]*estimativa_semana_seguinte
  
  data_semana_seguinte = vendas_tabaco[nrow(vendas_tabaco), 'Data']+7
  data_duassemanas_depois = vendas_tabaco[nrow(vendas_tabaco), 'Data']+14
  data_tressemanas_depois = vendas_tabaco[nrow(vendas_tabaco), 'Data']+21
  data_quatrosemanas_depois = vendas_tabaco[nrow(vendas_tabaco), 'Data']+28
  
  print(paste("A estimativa do número de tabacos demandados na próxima semana é:",round(as.numeric(estimativa_semana_seguinte), 2),"tabacos."))
  print(paste("A estimativa do número de tabacos demandados daqui a duas semanas é:",round(as.numeric(estimativa_duassemanas_depois), 2),"tabacos."))
  print(paste("A estimativa do número de tabacos demandados daqui a três semanas é:",round(as.numeric(estimativa_tressemanas_depois), 2),"tabacos."))
  print(paste("A estimativa do número de tabacos demandados daqui a quatro semanas é:",round(as.numeric(estimativa_quatrosemanas_depois), 2),"tabacos."))
  
  
  previsoes = c(estimativa_semana_seguinte, estimativa_duassemanas_depois, estimativa_tressemanas_depois, estimativa_quatrosemanas_depois)%>%
    as.numeric()
  
  datas_futuras = c(data_semana_seguinte, data_duassemanas_depois, data_tressemanas_depois, data_quatrosemanas_depois)%>%
    as.character()
  
  forecast = cbind(datas_futuras, previsoes)%>%
    as.data.frame()
  
  forecast$datas_futuras = as.Date(forecast$datas_futuras)
 
  df_vendas = vendas_tabaco%>%
    dplyr::select(Data, vendas_tabaco)%>%
    dplyr::rename(datas_futuras = 'Data', previsoes = 'vendas_tabaco')
  
  df_tabaco = rbind(df_vendas, forecast)
  
  df_tabaco = df_tabaco%>%
    dplyr::mutate(categoria = if_else(index(df_tabaco) > 29, "previsao", "realizado"))
  
  df_tabaco$previsoes = as.numeric(df_tabaco$previsoes)
  
  vendas = (
    ggplot(df_tabaco, aes(x = datas_futuras))+
      geom_line(mapping = aes(y = previsoes, colour = categoria))+
      scale_x_date(breaks = date_breaks('2 weeks'), labels = date_format('%d/%b/%Y'))+
      scale_y_continuous(breaks = seq(0, 16, 2))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      labs(x = '', y = '', title = 'Quantidade semanal de tabaco vendido e Correlograma das vendas'))
  
  pacf = forecast::ggPacf(vendas_tabaco$vendas_tabaco)

  correlogramas = gridExtra::grid.arrange(vendas, pacf, nrow = 2)
  
  return(correlogramas)
  
}

