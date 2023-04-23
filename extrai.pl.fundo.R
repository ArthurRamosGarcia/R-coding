

library(writexl)
library(magrittr)
library(readxl)
library(dplyr)
library(stringr)

setwd('C:/Users/Arthur/Desktop/Carteira teste/fundo x') 

extrai_patrimonio_carteira_fundo = function(carteira){
  
matriz_excel = readxl::read_excel(path_excel)  
  
for(i in 1:nrow(matriz_excel)){
  for(j in 1:ncol(matriz_excel))
    if(matriz_excel[i, j] == "Patrimônio"){
      patrimonio = matriz_excel[i, j+1][1]
    }
}

return(patrimonio)

}

gera.dados.pl.fundo.datasmov = function(){

path_carteiras = 'C:/Users/Arthur/Desktop/Carteira teste/fundo x'
  
passivos = readxl::read_excel('C:/Users/Arthur/Desktop/passivos fundo x.xlsx')
  
datas_mov = passivos$Data  
  
datas = data.frame(nrow(passivos),1)%>%
  cbind(datas_mov)

patrimonios = data.frame(nrow(passivos)[1], 1)

carteiras_fundox = list.files(path_carteiras)

i = 1

for(carteira in carteiras_fundox){
  data_carteira = str_extract(carteira, '[0-9]{8}')
  data_carteira = dmy(data_carteira)
  if(data_carteira %in% patrimonios$datas_mov){
    patrimonio_dia_carteira = extrai_patrimonio_fundo(carteira)
    patrimonios[i, 1] = patrimonio_dia_carteira
    
    i = i + 1
    
    stopifnot(i < nrow(patrimonios) | i == nrow(patrimonios))
  }
}

dados = cbind(datas, patrimonios)

WriteXLS::WriteXLS(dados, 'C:/Users/Arthur/Desktop/Carteira teste/fundo x/PatrimônioLíquido_datasmov.xls')

}
