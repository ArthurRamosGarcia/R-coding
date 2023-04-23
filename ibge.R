
library(sidrar)
library(dplyr)
library(ggplot2)
library(magrittr)
library(gridExtra)
library(tsibble)


gera.graficos.ibge = function(){
  
  # Produção Física Industrial por Atividades Industrais no ano de 2022  
  
  dados_sidra = sidrar::get_sidra(api = '/t/8159/n1/all/v/11599/p/last%2010/c544/all/d/v11599%205')%>%
    dplyr::mutate(mes = paste0(substr(dados_sidra$`Mês (Código)`, nchar(dados_sidra[1, 10])-5, nchar(dados_sidra[1, 10])-2),'-', substr(dados_sidra$`Mês (Código)`, nchar(dados_sidra[1, 10])-1, nchar(dados_sidra[1, 10])),'-', '01'))
  
  selic = GetBCBData::gbcbd_get_series(id = 4390, use.memoise = F)%>%
    dplyr::filter(ref.date <= '2022-10-01')%>%
    dplyr::filter(ref.date >= '2022-01-01')%>%
    dplyr::select(value)%>%
    dplyr::rename(selic = value)
  
  dados_sidra$mes = as.Date(dados_sidra$mes)
  
  dados_sidra_geral = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '1 Indústria geral')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
  dados_sidra_extrativa = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '2 Indústrias extrativas')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_transformacao = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3 Indústrias de transformação')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_alimentos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.10 Fabricação de produtos alimentícios')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_bebidas = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.11 Fabricação de bebidas')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_fumo = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.12 Fabricação de produtos do fumo')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_texteis = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.13 Fabricação de produtos têxteis')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_vestuario = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.14 Confecção de artigos do vestuário e acessórios')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_couros = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.15 Preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_madeira = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.16 Fabricação de produtos de madeira')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_celulose = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.17 Fabricação de celulose, papel e produtos de papel')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_gravacoes = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.18 Impressão e reprodução de gravações')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_derivadospetroleo = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.19 Fabricação de coque, de produtos derivados do petróleo e de biocombustíveis')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_saboes = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.20B Fabricação de sabões, detergentes, produtos de limpeza, cosméticos, produtos de perfumaria e de higiene pessoal')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_produtosquimicos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.20C Fabricação de outros produtos químicos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_farmoquimicos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.21 Fabricação de produtos farmoquímicos e farmacêuticos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_borracha = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.22 Fabricação de produtos de borracha e de material plástico')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_minerais = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.23 Fabricação de produtos de minerais não metálicos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_metalurgia = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.24 Metalurgia')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_metal = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.25 Fabricação de produtos de metal, exceto máquinas e equipamentos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_informatica = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.26 Fabricação de equipamentos de informática, produtos eletrônicos e ópticos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_materiaiseletricos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.27 Fabricação de máquinas, aparelhos e materiais elétricos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_maquinas = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.28 Fabricação de máquinas e equipamentos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_veiculos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.29 Fabricação de veículos automotores, reboques e carrocerias')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_equipamentostransporte = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.30 Fabricação de outros equipamentos de transporte, exceto veículos automotores')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_moveis = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.31 Fabricação de móveis')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_produtosdiversos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.32 Fabricação de produtos diversos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  dados_sidra_manutencao = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.33 Manutenção, reparação e instalação de máquinas e equipamentos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)
  
  sidra_dados= list(dados_sidra_geral, 
       dados_sidra_extrativa, 
       dados_sidra_transformacao, 
       dados_sidra_alimentos, 
       dados_sidra_bebidas, 
       dados_sidra_fumo, 
       dados_sidra_texteis, 
       dados_sidra_vestuario, 
       dados_sidra_couros, 
       dados_sidra_madeira, 
       dados_sidra_celulose, 
       dados_sidra_gravacoes, 
       dados_sidra_derivadospetroleo,  
       dados_sidra_saboes, 
       dados_sidra_produtosquimicos,
       dados_sidra_farmoquimicos,
       dados_sidra_borracha,
       dados_sidra_minerais,
       dados_sidra_metalurgia,
       dados_sidra_metal,
       dados_sidra_informatica,
       dados_sidra_materiaiseletricos,
       dados_sidra_maquinas,
       dados_sidra_veiculos,
       dados_sidra_equipamentostransporte,
       dados_sidra_moveis,
       dados_sidra_produtosdiversos,
       dados_sidra_manutencao)
  
  for(dataframe in sidra_dados){
    dplyr::mutate(dataframe, lag1 = dplyr::lag(Valor, 1), lag2 = dplyr::lag(Valor, 2))%>%
      tidyr::drop_na()
  }
  
  for(dataframe in sidra_dados){
    modelo = stats::lm(Valor~lag1+lag2, data = dataframe)
  }
  
  g1 = ggplot(dados_sidra_geral, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Geral', x = '', y = '')+theme_light()
  g2 = ggplot(dados_sidra_extrativa, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Extrativa', x = '', y = '')+theme_light()
  g3 = ggplot(dados_sidra_transformacao, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Transformacao', x = '', y = '')+theme_light()
  g4 = ggplot(dados_sidra_alimentos, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Alimentos', x = '', y = '')+theme_light()
  g5 = ggplot(dados_sidra_bebidas, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Bebidas', x = '', y = '')+theme_light()
  g6 = ggplot(dados_sidra_fumo, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Fumo', x = '', y = '')+theme_light()
  g7 = ggplot(dados_sidra_texteis, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Texteis', x = '', y = '')+theme_light()
  g8 = ggplot(dados_sidra_vestuario, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Vestuario', x = '', y = '')+theme_light()
  g9 = ggplot(dados_sidra_couros, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Couros', x = '', y = '')+theme_light()
  g10 = ggplot(dados_sidra_madeira, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Madeira', x = '', y = '')+theme_light()
  g11 = ggplot(dados_sidra_celulose, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Celulose', x = '', y = '')+theme_light()
  g12 = ggplot(dados_sidra_gravacoes, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Gravacoes', x = '', y = '')+theme_light()
  g13 = ggplot(dados_sidra_derivadospetroleo, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Derivadospetroleo', x = '', y = '')+theme_light()
  g14 = ggplot(dados_sidra_saboes, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Saboes', x = '', y = '')+theme_light()
  g15 = ggplot(dados_sidra_produtosquimicos, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Produtosquimicos', x = '', y = '')+theme_light()
  g16 = ggplot(dados_sidra_farmoquimicos, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Farmoquimicos', x = '', y = '')+theme_light()
  g17 = ggplot(dados_sidra_borracha, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Borracha', x = '', y = '')+theme_light()
  g18 = ggplot(dados_sidra_minerais, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Minerais', x = '', y = '')+theme_light()
  g19 = ggplot(dados_sidra_metalurgia, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Metalurgia', x = '', y = '')+theme_light()
  g20 = ggplot(dados_sidra_metal, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Metal', x = '', y = '')+theme_light()
  g21 = ggplot(dados_sidra_informatica, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'Informatica', x = '', y = '')+theme_light()
  g22 = ggplot(dados_sidra_materiaiseletricos, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'materiaiseletricos', x = '', y = '')+theme_light()
  g23 = ggplot(dados_sidra_maquinas, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'maquinas', x = '', y = '')+theme_light()
  g24 = ggplot(dados_sidra_veiculos, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'veiculos', x = '', y = '')+theme_light()
  g25 = ggplot(dados_sidra_equipamentostransporte, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'equipamentostransporte', x = '', y = '')+theme_light()
  g26 = ggplot(dados_sidra_moveis, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'moveis', x = '', y = '')+theme_light()
  g27 = ggplot(dados_sidra_produtosdiversos, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'produtosdiversos', x = '', y = '')+theme_light()
  g28 = ggplot(dados_sidra_manutencao, aes(x = mes))+geom_line(mapping = aes(y = Valor))+labs(title = 'manutencao', x = '', y = '')+theme_light()
  
  
  graficos = gridExtra::grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20,g21,g22,g23,g24,g25,g26,g27,g28,nrow = 4, ncol = 7)
  
  dados_sidra_geral = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '1 Indústria geral')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_extrativa = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '2 Indústrias extrativas')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_transformacao = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3 Indústrias de transformação')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_alimentos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.10 Fabricação de produtos alimentícios')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_bebidas = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.11 Fabricação de bebidas')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_fumo = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.12 Fabricação de produtos do fumo')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_texteis = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.13 Fabricação de produtos têxteis')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_vestuario = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.14 Confecção de artigos do vestuário e acessórios')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_couros = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.15 Preparação de couros e fabricação de artefatos de couro, artigos para viagem e calçados')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_madeira = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.16 Fabricação de produtos de madeira')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_celulose = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.17 Fabricação de celulose, papel e produtos de papel')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_gravacoes = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.18 Impressão e reprodução de gravações')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_derivadospetroleo = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.19 Fabricação de coque, de produtos derivados do petróleo e de biocombustíveis')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_saboes = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.20B Fabricação de sabões, detergentes, produtos de limpeza, cosméticos, produtos de perfumaria e de higiene pessoal')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_produtosquimicos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.20C Fabricação de outros produtos químicos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_farmoquimicos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.21 Fabricação de produtos farmoquímicos e farmacêuticos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_borracha = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.22 Fabricação de produtos de borracha e de material plástico')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_minerais = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.23 Fabricação de produtos de minerais não metálicos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_metalurgia = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.24 Metalurgia')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_metal = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.25 Fabricação de produtos de metal, exceto máquinas e equipamentos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_informatica = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.26 Fabricação de equipamentos de informática, produtos eletrônicos e ópticos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_materiaiseletricos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.27 Fabricação de máquinas, aparelhos e materiais elétricos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_maquinas = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.28 Fabricação de máquinas e equipamentos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_veiculos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.29 Fabricação de veículos automotores, reboques e carrocerias')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_equipamentostransporte = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.30 Fabricação de outros equipamentos de transporte, exceto veículos automotores')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_moveis = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.31 Fabricação de móveis')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_produtosdiversos = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.32 Fabricação de produtos diversos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  dados_sidra_manutencao = dados_sidra%>%
    dplyr::filter(dados_sidra$`Seções e atividades industriais (CNAE 2.0)` == '3.33 Manutenção, reparação e instalação de máquinas e equipamentos')%>%
    dplyr::select(Valor, `Seções e atividades industriais (CNAE 2.0)`, mes)%>%
    dplyr::mutate(diff = tsibble::difference(Valor))%>%
    cbind(selic)
  

  
  d1 = ggplot(dados_sidra_geral)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Geral', x = 'selic', y = 'industria geral')+theme_light()
  d2 = ggplot(dados_sidra_extrativa)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Extrativa', x = 'selic', y = 'extrativa')+theme_light()
  d3 = ggplot(dados_sidra_transformacao)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Transformacao', x = 'selic', y = 'transformacao')+theme_light()
  d4 = ggplot(dados_sidra_alimentos)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Alimentos', x = 'selic', y = 'alimentos')+theme_light()
  d5 = ggplot(dados_sidra_bebidas)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Bebidas', x = 'selic', y = 'bebidas')+theme_light()
  d6 = ggplot(dados_sidra_fumo)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Fumo', x = 'selic', y = 'fumo')+theme_light()
  d7 = ggplot(dados_sidra_texteis)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Texteis', x = 'selic', y = 'texteis')+theme_light()
  d8 = ggplot(dados_sidra_vestuario)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Vestuario', x = 'selic', y = 'vestuario')+theme_light()
  d9 = ggplot(dados_sidra_couros)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Couros', x = 'selic', y = 'couros')+theme_light()
  d10 = ggplot(dados_sidra_madeira)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Madeira', x = 'selic', y = 'madeira')+theme_light()
  d11 = ggplot(dados_sidra_celulose)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Celulose', x = 'selic', y = 'celulose')+theme_light()
  d12 = ggplot(dados_sidra_gravacoes)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Gravacoes', x = 'selic', y = 'gravacoes')+theme_light()
  d13 = ggplot(dados_sidra_derivadospetroleo)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Derivadospetroleo', x = 'selic', y = 'derivadospetroleo')+theme_light()
  d14 = ggplot(dados_sidra_saboes)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Saboes', x = 'selic', y = 'Sabões')+theme_light()
  d15 = ggplot(dados_sidra_produtosquimicos)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Produtosquimicos', x = 'selic', y = 'Produtos Químicos')+theme_light()
  d16 = ggplot(dados_sidra_farmoquimicos)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Farmoquimicos', x = 'selic', y = 'Farmoquímicos')+theme_light()
  d17 = ggplot(dados_sidra_borracha)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Borracha', x = 'selic', y = 'Borracha')+theme_light()
  d18 = ggplot(dados_sidra_minerais)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Minerais', x = 'selic', y = 'Minerais')+theme_light()
  d19 = ggplot(dados_sidra_metalurgia)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Metalurgia', x = 'selic', y = 'Metalurgia')+theme_light()
  d20 = ggplot(dados_sidra_metal)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Metal', x = 'selic', y = 'Metal')+theme_light()
  d21 = ggplot(dados_sidra_informatica)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'Informatica', x = 'selic', y = 'Informatica')+theme_light()
  d22 = ggplot(dados_sidra_materiaiseletricos)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'materiaiseletricos', x = 'selic', y = 'Materiais Elétricos')+theme_light()
  d23 = ggplot(dados_sidra_maquinas)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'maquinas', x = 'selic', y = 'Maquinas')+theme_light()
  d24 = ggplot(dados_sidra_veiculos)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'veiculos', x = 'selic', y = 'Veóiculos')+theme_light()
  d25 = ggplot(dados_sidra_equipamentostransporte)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'equipamentostransporte', x = 'selic', y = 'Equipamentos de Transporte')+theme_light()
  d26 = ggplot(dados_sidra_moveis)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'moveis', x = 'selic', y = 'Móveis')+theme_light()
  d27 = ggplot(dados_sidra_produtosdiversos)+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'produtosdiversos', x = 'selic', y = 'Produtos Diversos')+theme_light()
  d28 = ggplot(dados_sidra_manutencao, aes(x = mes))+geom_point(mapping = aes(x = selic, y = diff))+labs(title = 'manutencao', x = 'selic', y = 'Manutenção')+theme_light()
  
  
  graficos = gridExtra::grid.arrange(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12,d13,d14,d15,d16,d17,d18,d19,d20,d21,d22,d23,d24,d25,d26,d27,d28,nrow = 4, ncol = 7)
  
  
  return(graficos)
  
}
