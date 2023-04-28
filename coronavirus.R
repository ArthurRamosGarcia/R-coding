#############################################################################
################### INFECCÃO E MORTALIDADE DURANTE A PANDEMIA 


vacination = covid19.analytics::covid19.vaccination()

lubridate::day(vacination$date)=01

g1=vacination%>%
  dplyr::filter(location == 'United Kingdom' | location == 'Chile' | 
                  location == 'Portugal' | location == 'Spain' |
                  location == 'Uruguay' | location == 'Ireland' |
                  location == 'United States' | location == 'Italy' |
                  location == 'France' | location == 'Russia' |
                  location == 'China' | location == 'Australia' |
                  location == 'Brazil' | location == 'Finland' | 
                  location == 'Angola' | location == 'Canada' |
                  location == 'Germany' | location == 'Japan')%>%
  ggplot(aes(x=date, y = daily_vaccinations_per_million))+
  geom_col(aes(fill=location))+
  scale_x_date(breaks = date_breaks('1 month'), labels = date_format('%b/%Y'))+
  theme_classic()+
  theme(legend.position = 'top',
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = c("red", "green", 
                                    "blue", 'black', 
                                    'yellow', 'grey', 
                                    'orange', 'purple', 
                                    'pink', 'violet', 
                                    'darkblue', 'darkgreen', 
                                    'darkred', 'darkorange', 
                                    'darkviolet', 'mediumaquamarine', 
                                    'wheat', 'steelblue'))+
                                      labs(title = 'Evolução do Desempenho Mundial na Introdução das Vacinas',
                                           subtitle = 'Número de vacinados diariamente por Milhão de Pessoas',
                                           x='', y = '',caption = 'Fonte: Elaboração Própria com dados da OMS')

covid19br = covid19brazil::brazil_municipality

lubridate::day(covid19br$date) = 01

covid19br$date = as.Date(covid19br$date)


covid19_ac=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='AC')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Acre')

pop_ac=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='AC')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_ac = cbind(covid19_ac, pop_ac)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_al=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='AL')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Alagoas')

pop_al=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='AL')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_al = cbind(covid19_al, pop_al)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_am=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='AM')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Amazonas')

pop_am=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='AM')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_am = cbind(covid19_am, pop_am)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_ap=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='AP')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Amapá')

pop_ap=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='AP')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_ap = cbind(covid19_ap, pop_ap)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)


covid19_ba=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='BA')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Bahia')

pop_ba=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='BA')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_ba = cbind(covid19_ba, pop_ba)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)


covid19_ce=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='CE')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Ceará')

pop_ce=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='CE')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_ce = cbind(covid19_ce, pop_ce)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_df=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='DF')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Distrito Federal')

pop_df=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='DF')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_df = cbind(covid19_df, pop_df)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_es=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='ES')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Espírito Santo')

pop_es=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='ES')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_es = cbind(covid19_es, pop_es)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_go=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='GO')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Goiás')

pop_go=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='GO')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_go = cbind(covid19_go, pop_go)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_ma=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='MA')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Manaus')

pop_ma=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='MA')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_ma = cbind(covid19_ma, pop_ma)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_mg=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='MG')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Minas Gerais')

pop_mg=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='MG')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_mg = cbind(covid19_mg, pop_mg)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_ms=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='MS')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Mato Grosso do Sul')

pop_ms=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='MS')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_ms = cbind(covid19_ms, pop_ms)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_mt=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='MT')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Mato Grosso')

pop_mt=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='MT')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_mt = cbind(covid19_mt, pop_mt)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_pa=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PA')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Pará')

pop_pa=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='PA')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_pa = cbind(covid19_pa, pop_pa)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_pb=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PB')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Paraibá')

pop_pb=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='PB')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_pb = cbind(covid19_pb, pop_pb)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_pe=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PE')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Pernambuco')

pop_pe=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='PE')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_pe = cbind(covid19_pe, pop_pe)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_pi=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PI')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Piauí')

pop_pi=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='PI')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_pi = cbind(covid19_pi, pop_pi)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_pr=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PR')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Paraná')

pop_pr=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='PR')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_pr = cbind(covid19_pr, pop_pr)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_rj=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RJ')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Rio de Janeiro')

pop_rj=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='RJ')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_rj = cbind(covid19_rj, pop_rj)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_rn=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RN')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Rio Grande do Norte')

pop_rn=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='RN')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_rn = cbind(covid19_rn, pop_rn)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_ro=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RO')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Rondônia')

pop_ro=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='RO')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_ro = cbind(covid19_ro, pop_ro)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_rr=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RR')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Roraima')

pop_rr=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='RR')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_rr = cbind(covid19_rr, pop_rr)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_rs=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RS')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Rio Grande do Sul')

pop_rs=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='RS')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_rs = cbind(covid19_rs, pop_rs)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_sc=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='SC')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Santa Catarina')

pop_sc=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='SC')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_sc = cbind(covid19_sc, pop_sc)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_se=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='SE')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Sergipe')

pop_se=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='SE')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_se = cbind(covid19_se, pop_se)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_sp=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='SP')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='São Paulo')

pop_sp=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='SP')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_sp = cbind(covid19_sp, pop_sp)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

covid19_to=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='TO')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(casos=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Tocantins')

pop_to=covid19br%>%
  dplyr::select(state, date, population)%>%
  dplyr::filter(state=='TO')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(pop=sum(population), n = n())%>%
  as.data.frame()%>%
  dplyr::select(pop)

covid19_to = cbind(covid19_to, pop_to)%>%
  dplyr::mutate(perc_pop_infec = casos / pop)

dados = rbind(covid19_ac,covid19_al,
              covid19_am,covid19_ap,
              covid19_ba,covid19_ce,
              covid19_df,covid19_es,
              covid19_go,covid19_ma,
              covid19_mg,covid19_ms,
              covid19_mt,covid19_pa,
              covid19_pb,covid19_pe,
              covid19_pi,covid19_pr,
              covid19_rj,covid19_rn,
              covid19_ro,covid19_rr,
              covid19_rs,covid19_sc,
              covid19_se,covid19_sp,
              covid19_to)

dados$data = paste0(dados$data,'-01')

dados$data = as.Date(dados$data)

dados%>%
  as.data.frame()%>%
  ggplot(aes(x=data, y =perc_pop_infec))+
  geom_col(aes(fill=estado))+
  theme_classic()+
  theme(legend.position = 'top',
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_date(breaks=date_breaks('1 month'), labels = date_format('%b/%Y'))+
  scale_fill_manual(values = c("red", "green", "blue", 
                                    'black', 'yellow', 'grey', 
                                    'orange', 'purple', 'pink', 
                                    'violet', 'darkblue', 'darkgreen', 
                                    'darkred', 'darkorange', 'darkviolet', 
                                    'mediumaquamarine', 'wheat', 'steelblue', 
                                    'violetred', 'slateblue', 'skyblue', 'snow2', 
                                    'tomato2', 'salmon', 'rosybrown2', 
                                    'olivedrab2', 'limegreen'))+
  labs(title='Evolução da Pandemia no Brasil', subtitle = 'Parcela da População Contaminada por Estado Brasileiro', x='', y ='',
       caption = 'Fonte: Elaboração Própria com dados do Ministério da Saúde')

covid19mort_ac=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='AC')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Acre')

pop_ac=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='AC')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_ac = cbind(covid19mort_ac, pop_ac)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_al=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='AL')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Alagoas')

pop_al=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='AL')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_al = cbind(covid19mort_al, pop_al)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_am=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='AM')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Amazonas')

pop_am=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='AM')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_am = cbind(covid19mort_am, pop_am)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_ap=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='AP')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Amapá')

pop_ap=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='AP')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_ap = cbind(covid19mort_ap, pop_ap)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)


covid19mort_ba=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='BA')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Bahia')

pop_ba=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='BA')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_ba = cbind(covid19mort_ba, pop_ba)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)


covid19mort_ce=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='CE')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Ceará')

pop_ce=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='CE')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_ce = cbind(covid19mort_ce, pop_ce)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_df=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='DF')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Distrito Federal')

pop_df=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='DF')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_df = cbind(covid19mort_df, pop_df)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_es=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='ES')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Espírito Santo')

pop_es=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='ES')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_es = cbind(covid19mort_es, pop_es)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_go=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='GO')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Goiás')

pop_go=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='GO')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_go = cbind(covid19mort_go, pop_go)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_ma=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='MA')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Manaus')

pop_ma=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='MA')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_ma = cbind(covid19mort_ma, pop_ma)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_mg=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='MG')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Minas Gerais')

pop_mg=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='MG')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_mg = cbind(covid19mort_mg, pop_mg)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_ms=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='MS')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Mato Grosso do Sul')

pop_ms=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='MS')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_ms = cbind(covid19mort_ms, pop_ms)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_mt=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='MT')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Mato Grosso')

pop_mt=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='MT')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_mt = cbind(covid19mort_mt, pop_mt)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_pa=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PA')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Pará')

pop_pa=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='PA')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_pa = cbind(covid19mort_pa, pop_pa)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_pb=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PB')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Paraibá')

pop_pb=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='PB')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_pb = cbind(covid19mort_pb, pop_pb)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_pe=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PE')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Pernambuco')

pop_pe=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='PE')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_pe = cbind(covid19mort_pe, pop_pe)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_pi=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PI')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Piauí')

pop_pi=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='PI')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_pi = cbind(covid19mort_pi, pop_pi)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_pr=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='PR')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Paraná')

pop_pr=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='PR')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_pr = cbind(covid19mort_pr, pop_pr)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_rj=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RJ')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Rio de Janeiro')

pop_rj=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='RJ')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_rj = cbind(covid19mort_rj, pop_rj)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_rn=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RN')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Rio Grande do Norte')

pop_rn=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='RN')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_rn = cbind(covid19mort_rn, pop_rn)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_ro=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RO')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Rondônia')

pop_ro=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='RO')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_ro = cbind(covid19mort_ro, pop_ro)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_rr=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RR')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Roraima')

pop_rr=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='RR')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_rr = cbind(covid19mort_rr, pop_rr)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_rs=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='RS')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Rio Grande do Sul')

pop_rs=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='RS')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_rs = cbind(covid19mort_rs, pop_rs)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_sc=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='SC')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Santa Catarina')

pop_sc=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='SC')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_sc = cbind(covid19mort_sc, pop_sc)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_se=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='SE')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Sergipe')

pop_se=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='SE')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_se = cbind(covid19mort_se, pop_se)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_sp=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='SP')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='São Paulo')

pop_sp=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='SP')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_sp = cbind(covid19mort_sp, pop_sp)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

covid19mort_to=covid19br%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::filter(state=='TO')%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(mortes=sum(accumDeaths), n = n())%>%
  as.data.frame()%>%
  dplyr::mutate(estado='Tocantins')

pop_to=covid19br%>%
  dplyr::select(state, date, accumCases)%>%
  dplyr::filter(state=='TO')%>%
  dplyr::mutate(data = substr(date, 1, 7))%>%
  dplyr::group_by(data)%>%
  dplyr::summarise(accumCases=sum(accumCases), n = n())%>%
  as.data.frame()%>%
  dplyr::select(accumCases)

covid19mort_to = cbind(covid19mort_to, pop_to)%>%
  dplyr::mutate(mortalidade = mortes / accumCases)

dados_mort = rbind(covid19mort_ac,covid19mort_al,
                   covid19mort_am,covid19mort_ap,
                   covid19mort_ba,covid19mort_ce,
                   covid19mort_df,covid19mort_es,
                   covid19mort_go,covid19mort_ma,
                   covid19mort_mg,covid19mort_ms,
                   covid19mort_mt,covid19mort_pa,
                   covid19mort_pb,covid19mort_pe,
                   covid19mort_pi,covid19mort_pr,
                   covid19mort_rj,covid19mort_rn,
                   covid19mort_ro,covid19mort_rr,
                   covid19mort_rs,covid19mort_sc,
                   covid19mort_se,covid19mort_sp,
                   covid19mort_to)

dados_mort$data= as.Date(dados$data)

dados_mort$data = paste0(dados_mort$data,'-01')

dados_mort%>%
  dplyr::filter(data < '2022-06-01')%>%
  as.data.frame()%>%
  ggplot(aes(x=data, y =mortalidade))+
  geom_col(aes(fill=estado))+
  theme_classic()+
  theme(legend.position = 'top',
        legend.key.size = unit(0.3, 'cm'),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_date(breaks=date_breaks('1 month'), labels = date_format('%b/%Y'))+
  scale_fill_manual(values = c("red", "green", "blue", 'black', 
                                    'yellow', 'grey', 'orange', 'purple', 
                                    'pink', 'violet', 'darkblue', 'darkgreen', 
                                    'darkred', 'darkorange', 'darkviolet', 'mediumaquamarine', 
                                    'wheat', 'steelblue', 'violetred', 'slateblue', 
                                    'skyblue', 'snow2', 'tomato2', 'salmon', 
                                    'rosybrown2', 'olivedrab2', 'limegreen'))+
  labs(title='Evolução da Pandemia no Brasil', subtitle = 'Taxa de Mortalidade da População Contaminada por Estado Brasileiro', x='', y ='',
       caption = 'Fonte: Elaboração Própria com dados do Ministério da Saúde')


