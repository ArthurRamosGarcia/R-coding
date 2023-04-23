

get.financial.statements = function(cvm_code, first_year, last_year){
  
  statements = GetDFPData2::get_dfp_data(
    companies_cvm_codes = cvm_code,
    first_year = first_year,
    last_year = last_year,
    use_memoise = F,
    clean_data = T,
    cache_folder = tempdir(),
    type_docs = c('DRE', 'BPA', 'BPP', 'DFC_MI'),
    type_format = 'con')
  
  bpa = statements$`DF Consolidado - Balanço Patrimonial Ativo`
  bpp = statements$`DF Consolidado - Balanço Patrimonial Passivo`
  dfc = statements$`DF Consolidado - Demonstração do Fluxo de Caixa (Método Indireto)`
  dre = statements$`DF Consolidado - Demonstração do Resultado`
  
  writexl::write_xlsx(bpa, 'C:/Users/Arthur/Desktop/REP OPERACIONAL/Fundos 555/selection/Balanço Patrimonial Ativo.xlsx')
  writexl::write_xlsx(bpp, 'C:/Users/Arthur/Desktop/REP OPERACIONAL/Fundos 555/selection/Balanço Patrimonial Passivo.xlsx')
  writexl::write_xlsx(dfc, 'C:/Users/Arthur/Desktop/REP OPERACIONAL/Fundos 555/selection/Demonstrativo de Fluxo de Caixa.xlsx')
  writexl::write_xlsx(dre, 'C:/Users/Arthur/Desktop/REP OPERACIONAL/Fundos 555/selection/Demonstrativo de Resultado do Exercício.xlsx')

  bpa = bpa%>%
    dplyr::filter(DS_CONTA == 'Ativo Total' |
                    DS_CONTA == 'Caixa e Equivalentes de Caixa' |
                    DS_CONTA == 'Ativos Financeiros' |
                    DS_CONTA == 'Tributos' |
                    DS_CONTA == 'Ativo Total' |
                    DS_CONTA == 'Investimentos' |
                    DS_CONTA == 'Ativo Total' |
                    DS_CONTA == 'Imobilizado' |
                    DS_CONTA == 'Intangível' |
                    DS_CONTA == 'Ativo Total')%>%
    dplyr::select(DT_REFER, 
                  GRUPO_DFP, 
                  DS_CONTA, 
                  VL_CONTA)
  
  bpp = bpp%>%
    dplyr::filter(DS_CONTA == 'Passivo Total' |
                    DS_CONTA == 'ProvisÃµes' |
                    DS_CONTA == 'Passivos Fiscais' |
                    DS_CONTA == 'Outros Passivos' |
                    DS_CONTA == 'Passivo Total' |
                    DS_CONTA == 'Patrimônio Líquido Consolidado')%>%
    dplyr::select(DT_REFER, 
                  GRUPO_DFP, 
                  DS_CONTA, 
                  VL_CONTA)
  
  dfc = dfc%>%
    dplyr::filter(DS_CONTA == 'Caixa Líquido das Atividades Operacionais' |
                    DS_CONTA == 'VariaÃ§Ã£o nos Ativos e Passivos' |
                    DS_CONTA == 'Caixa Líquido das Atividades de Investimento' |
                    DS_CONTA == 'Caixa Líquido das Atividades de Financiamento' |
                    DS_CONTA == 'Aumento (Redução) de Caixa e Equivalentes' |
                    DS_CONTA == 'Aumento (Redução) de Caixa e Equivalentes')%>%
    dplyr::select(DT_REFER, 
                  GRUPO_DFP, 
                  DS_CONTA, 
                  VL_CONTA)

dre_cds = c('3.01', '3.02', '3.03', '3.04', '3.05', '3.06', '3.07', '3.08', '3.09')
    
  dre = dre%>%
    dplyr::filter(CD_CONTA %in% dre_cds)%>%
    dplyr::select(DT_REFER, 
                  GRUPO_DFP, 
                  DS_CONTA, 
                  VL_CONTA)

  writexl::write_xlsx(bpa, 'C:/Users/Arthur/Desktop/REP OPERACIONAL/Fundos 555/selection/infosBPA.xlsx')
  writexl::write_xlsx(bpp, 'C:/Users/Arthur/Desktop/REP OPERACIONAL/Fundos 555/selection/infosBPP.xlsx')
  writexl::write_xlsx(dfc, 'C:/Users/Arthur/Desktop/REP OPERACIONAL/Fundos 555/selection/infosDFC.xlsx')
  writexl::write_xlsx(dre, 'C:/Users/Arthur/Desktop/REP OPERACIONAL/Fundos 555/selection/infosDRE.xlsx')
  
}

   
    
  