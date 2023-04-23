
library(sidrar)
library(dplyr)
library(magrittr)
library(ggplot2)
library(scales)


gera.grafico.barras.ipca.mensal.ibge = function(){

# IPCA - Variação Mensal 2022 (jan/22 --> dez/22)  
  
ipca = sidrar::get_sidra(api = '/t/7060/n1/all/v/63/p/last%2011/c315/7169,7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202')%>%
  dplyr::mutate(mes = as.Date(paste0(substr(ipca$`Mês (Código)`, nchar(ipca[1, 10])-5, nchar(ipca[1, 10])-2),'-', substr(ipca$`Mês (Código)`, nchar(ipca[1, 10])-1, nchar(ipca[1, 10])),'-', '01')))

ipca_alimentacaobebidas = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '1.Alimentação e bebidas')%>%
  dplyr::select(mes, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)
ipca_habitacao = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '2.Habitação')%>%
  dplyr::select(mes, Valor, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)
ipca_artigosresidencia = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '3.Artigos de residência')%>%
  dplyr::select(mes, Valor, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)
ipca_vestuario = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '4.Vestuário')%>%
  dplyr::select(mes, Valor, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)
ipca_transportes = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '5.Transportes')%>%
  dplyr::select(mes, Valor, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)
ipca_saude = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '6.Saúde e cuidados pessoais')%>%
  dplyr::select(mes, Valor, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)
ipca_despesaspessoais = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '7.Despesas pessoais')%>%
  dplyr::select(mes, Valor, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)
ipca_educacao = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '8.Educação')%>%
  dplyr::select(mes, Valor, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)
ipca_comunicacao = ipca%>%
  dplyr::filter(`Geral, grupo, subgrupo, item e subitem` == '9.Comunicação')%>%
  dplyr::select(mes, Valor, Valor, `Geral, grupo, subgrupo, item e subitem`)%>%
  dplyr::rename(Categoria = `Geral, grupo, subgrupo, item e subitem`)

dados = rbind(ipca_alimentacaobebidas, ipca_habitacao, ipca_artigosresidencia, ipca_vestuario, ipca_transportes, ipca_saude, ipca_despesaspessoais, ipca_educacao, ipca_comunicacao)

  grafico = dados%>%
    ggplot(aes(x = mes, y = Valor))+
    geom_col(aes(fill = Categoria))+
    theme(legend.position = 'top', legend.key.size = unit(0.3, 'cm'), legend.title = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
    scale_x_date(breaks = date_breaks('1 month'), labels = date_format('%b/%Y'))+
    scale_y_continuous(breaks = seq(-8, 12.5, 2))+
    labs(x = '', y = 'Variação Mensal (%)', title = 'Decomposição do IPCA por Categoria', caption = 'Fonte: IBGE')

return(grafico)

}