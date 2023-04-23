idaipca = readxl::read_xlsx('C:/Users/Arthur/Downloads/idaipca.xlsx', skip = 1)

idaipca = idaipca%>%
  dplyr::mutate(Ticker = substr(Código, 1, 4))


setores_economicos = readxl::read_xlsx('C:/Users/Arthur/Desktop/Setores das Empresas do IDAIPCA.xlsx')

debipca = inner_join(idaipca, setores_economicos, by = 'Ticker')%>%
  dplyr::select(Ticker, `Peso do Ativo (%)`, `Setor Econômico`)%>%
  tidyr::drop_na()


debipcautilidadespub = debipca%>%
  dplyr::filter(`Setor Econômico` == 'Utilidade Pública')
debipcaindustriais = debipca%>%
  dplyr::filter(`Setor Econômico` == 'Bens Industriais')
debipcaciclico = debipca%>%
  dplyr::filter(`Setor Econômico` == 'Consumo Cíclico')
debipcanaociclico = debipca%>%
  dplyr::filter(`Setor Econômico` == 'Consumo não Cíclico')
debipcagascombust = debipca%>%
  dplyr::filter(`Setor Econômico` == 'Petróleo, Gás e Biocombustíveis')
debipcamatbasicos = debipca%>%
  dplyr::filter(`Setor Econômico` == 'Materiais Básicos')
debipcaTI = debipca%>%
  dplyr::filter(`Setor Econômico` == 'Tecnologia da Informação')
debipcafinanceiro = debipca%>%
  dplyr::filter(`Setor Econômico` == 'Financeiro')


peso_utilidade_publica = sum(debipca$[debipca$`Setor Econômico` == `Utilidade Pública`),`Peso do Ativo (%)`])[debipca$`Setor Econômico` == `Utilidade Pública`])
peso_bens_industriais = sum(debipca$`Peso do Ativo (%)`[debipca$`Setor Econômico` == `Bens Industriais`])
peso_consumo_ciclico = sum(debipca$`Peso do Ativo (%)`[debipca$`Setor Econômico` == `Consumo Cíclico`])
peso_consumo_não_cíclico = sum(debipca$`Peso do Ativo (%)`[debipca$`Setor Econômico` == `Consumo não Cíclico`])
peso_petroleo_gas_combustíveis = sum(debipca$`Peso do Ativo (%)`[debipca$`Setor Econômico` == `Petróleo, Gás e Biocombustíveis`])
peso_materiais_basicos = sum(debipca$`Peso do Ativo (%)`[debipca$`Setor Econômico` == `Materiais Básicos`])
peso_tecnologia_informação = sum(debipca$`Peso do Ativo (%)`[debipca$`Setor Econômico` == `Tecnologia da Informação`])
peso_construcao_engenharia = sum(debipca$`Peso do Ativo (%)`[debipca$`Setor Econômico` == `Construção e Engenharia`])
peso_financeiro = sum(debipca$`Peso do Ativo (%)`[debipca$`Setor Econômico` == `Financeiro`])

pesos=c(peso_utilidade_publica,peso_bens_industriais,peso_consumo_ciclico,peso_consumo_não_cíclico,peso_petroleo_gas_combustíveis,peso_materiais_basicos,peso_tecnologia_informação,peso_construcao_engenharia,peso_financeiro)%>%
  as.numeric()
setores = c('Utilidade Pública', 'Bens Industriais', 'Consumo Cíclico', 'Consumo não Cíclico', 'Petróleo, Gás e Biocombustíveis', 'Materiais Básicos', 'Construção e Engenharia', 'Tecnologia da Informação', 'Financeiro')

sum(debipca$`Peso do Ativo (%)`)



for(i in 1:nrow(idaipca)){
  if(!idaipca[i, 'Ticker'] %in% setores_economicos$Ticker){
    print(idaipca[i, 'Ticker'])
    
  }
  
  
}

setores_economicos = setores_economicos%>%
  dplyr::filter(zoo::index(setores_economicos) < nrow(setores_economicos))


table(debipca$`Setor Econômico`)
