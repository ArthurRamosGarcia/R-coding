

gera.pesos.categorias.ifix = function(){

url = 'https://fiis.com.br/ifix/'

pg = rvest::read_html(url)

Dados = html_table(pg)%>%
  as.data.frame()

Dados_EQI = readxl::read_xlsx('C:/Users/Arthur/Downloads/Fundamentos FIIs.xlsx', sheet = 'Dados ClubeFII')%>%
  dplyr::rename(Ticker = `Código Negociação\r\n`)

df = dplyr::inner_join(Dados, Dados_EQI, by = 'Ticker')%>%
  dplyr::mutate(Segmento = ifelse(Categoria == 'Shopping/Varejo' | Categoria == 'Lajes Comerciais' | Categoria == 'Fundo de Fundos' | Categoria == 'Logisticos' | Categoria == 'Recebíveis ImobiliÃ¡rios', Categoria, "Outros"))

Segmentos = c('Shopping/Varejo', 'Lajes Comerciais', 'Fundo de Fundos', 'Logísticos', 'Recebíveis Imobiliários', 'Outros')

Pesos = c(table(df$Segmento[df$Segmento == 'Shopping/Varejo']) / length(df$Segmento) * 100, table(df$Segmento[df$Segmento == 'Lajes Comerciais']) / length(df$Segmento) * 100, table(df$Segmento[df$Segmento == 'Fundo de Fundos']) / length(df$Segmento) * 100, table(df$Segmento[df$Segmento == 'Logisticos']) / length(df$Segmento) * 100, table(df$Segmento[df$Segmento == 'Recebíveis Imobiliários']) / length(df$Segmento) * 100, table(df$Segmento[df$Segmento == 'Outros']) / length(df$Segmento) * 100)

df_final = cbind(Segmentos, Pesos)%>%
  as.data.frame()

df_final$Pesos = as.numeric(df_final$Pesos)
df_final$Pesos = round(df_final$Pesos, 2)

df_final = df_final%>%
  dplyr::mutate(Valor = paste(Segmentos,Pesos,'%'))

pie = pie(df_final$Pesos, labels = df_final$Valor, main = 'IFIX por Segmento')

dev.new(width = 10, height = 7)

return(pie)

}