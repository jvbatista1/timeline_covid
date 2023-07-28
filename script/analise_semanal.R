###### BIBLIOTECAS ######
library(readr)
detach(package::plyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)
library(zoo)

###### LEITURA DA BASE ######
# dadosbrasil <- read_excel(file.choose(), sheet = 'brasil')
# dadosbrasil <- dadosbrasil %>%
#   mutate(date = ymd(date))
# 
# dataset2 <- read_excel(file.choose(), sheet = 'estados_diario')
# dataset <- dataset %>%
#   mutate(date = ymd(date))
# 
# dataset <- read_csv('data/cases-brazil-states.csv')
# dataset <- dataset %>%
#   mutate(date = ymd(date))

dataset <- read_rds('data/casos_capitais.rds')
dataset <- dataset %>%
  mutate(date = ymd(date))

# dadosestadosfiltrados <- dataset %>% 
#   filter(state %in% c("CE","PE","BA", "PA","GO", "RS", "SP")) %>% 
#   arrange(state)

###### M?DIAS M?VEIS ######
# dadosestadosfiltrados$populacao <- (dadosestadosfiltrados$totalCases/dadosestadosfiltrados$totalCases_per_100k_inhabitants)*10000
# 
# dadosestadosfiltrados$rollmeannewCases <- rollmean(dadosestadosfiltrados$newCases, 7, fill = NA)
# dadosbrasil$rollmeannewCases <- rollmean(dadosbrasil$newCases, 7, fill = NA)
# 
# dadosestadosfiltrados$newCases_per_100k_inhabitants <- (dadosestadosfiltrados$newCases/dadosestadosfiltrados$populacao)*100000
# 
# dadosestadosfiltrados$rollmeannewCases_per_100k_inhabitants <- rollmean(dadosestadosfiltrados$newCases_per_100k_inhabitants, 7, fill = NA)
# dadosbrasil$rollmeannewCases_per_100k_inhabitants <- rollmean(dadosbrasil$newCases_per_100k_inhabitants, 7, fill = NA)

###### AGRUPANDO POR SEMANA ######
# dadosbrasil_weekly <- dadosbrasil %>% 
#   group_by(state, epi_week) %>% 
#   summarise(weekly_newCases = sum(newCases), date = first(date))
# 
# dadosestados_weekly <- dadosestadosfiltrados %>% 
#   group_by(state, epi_week) %>% 
#   summarise(weekly_newCases = sum(newCases), date = first(date))

###### INSERINDO VARIA??O PERCENTUAL ######
# dadosbrasil_weekly$var_percentual = NA
# 
# for(i in 1:length(dadosbrasil_weekly$weekly_newCases)){
#   dadosbrasil_weekly$var_percentual[i+1] = (dadosbrasil_weekly$weekly_newCases[i+1] - dadosbrasil_weekly$weekly_newCases[i])/dadosbrasil_weekly$weekly_newCases[i]
# }
# 
# dadosestados_weekly$var_percentual <- NA
# 
# dadosce <- filter(dadosestados_weekly, state == 'CE')
# dadosba <- filter(dadosestados_weekly, state == 'BA')
# dadospe <- filter(dadosestados_weekly, state == 'PE')
# dadospa <- filter(dadosestados_weekly, state == 'PA')
# dadosgo <- filter(dadosestados_weekly, state == 'GO')
# dadossp <- filter(dadosestados_weekly, state == 'SP')
# dadosrs <- filter(dadosestados_weekly, state == 'RS')
# 
# for(i in 1:length(dadosce$weekly_newCases)){
#   dadosce$var_percentual[i+1] = (dadosce$weekly_newCases[i+1] - dadosce$weekly_newCases[i])/dadosce$weekly_newCases[i]
# }
# for(i in 1:length(dadosba$weekly_newCases)){
#   dadosba$var_percentual[i+1] = (dadosba$weekly_newCases[i+1] - dadosba$weekly_newCases[i])/dadosba$weekly_newCases[i]
# }
# for(i in 1:length(dadospe$weekly_newCases)){
#   dadospe$var_percentual[i+1] = (dadospe$weekly_newCases[i+1] - dadospe$weekly_newCases[i])/dadospe$weekly_newCases[i]
# }
# for(i in 1:length(dadospa$weekly_newCases)){
#   dadospa$var_percentual[i+1] = (dadospa$weekly_newCases[i+1] - dadospa$weekly_newCases[i])/dadospa$weekly_newCases[i]
# }
# for(i in 1:length(dadosgo$weekly_newCases)){
#   dadosgo$var_percentual[i+1] = (dadosgo$weekly_newCases[i+1] - dadosgo$weekly_newCases[i])/dadosgo$weekly_newCases[i]
# }
# for(i in 1:length(dadossp$weekly_newCases)){
#   dadossp$var_percentual[i+1] = (dadossp$weekly_newCases[i+1] - dadossp$weekly_newCases[i])/dadossp$weekly_newCases[i]
# }
# for(i in 1:length(dadosrs$weekly_newCases)){
#   dadosrs$var_percentual[i+1] = (dadosrs$weekly_newCases[i+1] - dadosrs$weekly_newCases[i])/dadosrs$weekly_newCases[i]
# }
###### INSERINDO CONTADOR DE SEMANA CONSECUTIVA ######
# setDT(dadosbrasil_weekly)
# dadosbrasil_weekly[, consec_week := seq_len(.N), rleid(var_percentual > 0)]
# dadosbrasil_weekly[var_percentual <= 0, consec_week := NA]
# 
# dadosbrasil_weekly$iniciodeonda <- NA
# 
# for (i in length(dadosbrasil_weekly$consec_week)){
#   if(dadosbrasil_weekly$consec_week[i+2] == 3){
#     dadosbrasil_weekly$iniciodeonda[i] <- 1
#   }
# }
# 
# setDT(dadosce)
# dadosce[, consec_week := seq_len(.N), rleid(var_percentual > 0)]
# dadosce[var_percentual <= 0, consec_week := NA]
# 
# setDT(dadosba)
# dadosba[, consec_week := seq_len(.N), rleid(var_percentual > 0)]
# dadosba[var_percentual <= 0, consec_week := NA]
# 
# setDT(dadospe)
# dadospe[, consec_week := seq_len(.N), rleid(var_percentual > 0)]
# dadospe[var_percentual <= 0, consec_week := NA]
# 
# setDT(dadospa)
# dadospa[, consec_week := seq_len(.N), rleid(var_percentual > 0)]
# dadospa[var_percentual <= 0, consec_week := NA]
# 
# setDT(dadosgo)
# dadosgo[, consec_week := seq_len(.N), rleid(var_percentual > 0)]
# dadosgo[var_percentual <= 0, consec_week := NA]
# 
# setDT(dadossp)
# dadossp[, consec_week := seq_len(.N), rleid(var_percentual > 0)]
# dadossp[var_percentual <= 0, consec_week := NA]
# 
# setDT(dadosrs)
# dadosrs[, consec_week := seq_len(.N), rleid(var_percentual > 0)]
# dadosrs[var_percentual <= 0, consec_week := NA]

###### C?LCULO DA DIVIS?O DAS ONDAS ######

##### onda 1
division_of_waves1 <- dadosbrasil %>% 
  filter(date > ymd("2020-07-01") & date < ymd("2021-07-01"))

aux <- min(division_of_waves1$rollmeannewCases)
aux <- filter(dadosbrasil, rollmeannewCases == aux)
division_of_waves1 <- aux$date


##### onda 2
division_of_waves2 <- dadosbrasil %>% 
  filter(date > ymd("2021-07-01") & date < ymd("2022-06-27"))

aux <- min(division_of_waves2$rollmeannewCases)
aux <- filter(dadosbrasil, rollmeannewCases == aux)
division_of_waves2 <- aux$date

###### C?LCULO DOS PONTOS DE M?XIMO ######
# dadosestadosfiltrados%>%
#   filter(date>ymd('2020-07-12') & date<ymd("2021-09-23")) %>%
#   group_by(state) %>% 
#   mutate(rank = min_rank(desc(rollmeannewCases))) %>% 
#   select(state, date, rank, rollmeannewCases) %>%
#   filter(rank == 1)

dataset%>%
  filter(date>ymd('2020-07-12') & date<ymd("2021-09-23")) %>%
  group_by(administrative_area_level_3) %>% 
  mutate(rank = min_rank(desc(rollmeannewCases))) %>% 
  select(administrative_area_level_3, date, rank, rollmeannewCases) %>%
  filter(rank == 1)

# dadosestadosfiltrados%>%
#   filter(date>ymd('2021-09-23')) %>%
#   group_by(state) %>% 
#   mutate(rank = min_rank(desc(rollmeannewCases))) %>% 
#   select(state, date, rank, rollmeannewCases) %>%
#   filter(rank == 1)
  
dataset%>%
  filter(date>ymd('2021-09-23')) %>%
  group_by(administrative_area_level_3) %>% 
  mutate(rank = min_rank(desc(rollmeannewCases))) %>% 
  select(administrative_area_level_3, date, rank, rollmeannewCases) %>%
  filter(rank == 1)

###### GR?FICO BRASIL ######
graficobrasil <- dadosbrasil %>%
  ggplot(aes(x = date, y = rollmeannewCases_per_100k_inhabitants)) +
  geom_line(show.legend = F, color = "#00B6EB") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, size = 6))+
  ggtitle("BRASIL")+
  xlab("Data") +
  ylab('')+
  scale_x_date(limits = c(ymd("2019-03-01"), ymd("2022-06-28")),
               date_breaks = "1 month",
               date_labels = "%b %y",
               date_minor_breaks = "6 months")+
  geom_vline(xintercept = as.numeric(ymd("2019-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2019-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), col = "gray") +    ###### divisores dos semestres
  geom_vline(xintercept = as.numeric(ymd("2020-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2020-03-01")), col = "red")+      ###### Início da primeira onda
  geom_vline(xintercept = as.numeric(division_of_waves1), col = "red")+     ###### divis?es entre ondas
  geom_vline(xintercept = as.numeric(division_of_waves2), col = "red")+
  geom_vline(xintercept = as.numeric(ymd("2021-11-20")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2022-02-25")), col = "purple", linetype = "twodash")+
  geom_vline(xintercept = as.numeric(ymd("2019-08-05")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2019-12-15")), col = "purple", linetype = "twodash")+
  annotate(geom = "text", x = as.Date("2020-03-01"), y = 50, size = 2, label = "Início da \n 1a onda")+
  annotate(geom = "text", x = as.Date(division_of_waves1), y = 50, size = 2, label = "Fim da \n 1a onda")+
  annotate(geom = "text", x = as.Date(division_of_waves2), y = 50, size = 2, label = "Fim da \n 2a onda")+
  annotate(geom = "text", x = ymd("2021-11-20"), y = 60, size = 2, label = "Início coleta 2021")+
  annotate(geom = "text", x = ymd("2022-02-25"), y = 60, size = 2, label = "Fim coleta 2021")+
  annotate(geom = "text", x = ymd("2019-08-05"), y = 60, size = 2, label = "Início coleta 2019")+
  annotate(geom = "text", x = ymd("2019-12-15"), y = 60, size = 2, label = "Fim coleta 2019")

###### GR?FICO ESTADOS ######
#c("#F8766D", "#C49A00", "#53B400", "#00C094", "#00B6EB", "#A58AFF", "#FB61D7")
# dadosestado <- dadosestadosfiltrados %>% 
#   filter(state == "SP")

dadosestado <- dataset %>% 
  filter(administrative_area_level_3 == "Belém")

##### onda 1
division_of_waves1 <- dadosestado %>% 
  filter(date > ymd("2020-07-01") & date < ymd("2021-07-01"))

aux <- min(division_of_waves1$rollmeannewCases)
aux <- filter(dadosestado, rollmeannewCases == aux)
division_of_waves1 <- aux$date

##### onda 2
division_of_waves2 <- dadosestado %>% 
  filter(date > ymd("2021-07-01") & date < ymd("2022-03-01"))

aux <- min(division_of_waves2$rollmeannewCases)
aux <- filter(dadosestado, rollmeannewCases == aux)
division_of_waves2 <- aux$date

##### GR?FICO BAHIA #####
graficoba <- dadosestado %>%
  ggplot(aes(x = date, y = rollmeannewCases_per_100k_inhabitants)) +
  geom_line(show.legend = F, color = "#00B6EB") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, size = 6))+
  ggtitle("SALVADOR")+
  xlab("data") +
  ylab(' ')+
  scale_x_date(limits = c(ymd("2020-02-25"), ymd("2023-03-18")), date_breaks = "1 month", date_labels = "%b %y", date_minor_breaks = "6 months")+
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), col = "gray") +    ###### divisores dos semestres
  geom_vline(xintercept = as.numeric(ymd("2020-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-07-01")), col = "gray") +
  
  geom_vline(xintercept = as.numeric(ymd("2020-03-08")), col = "red", linetype = "dashed")+      ###### Início da primeira onda
  geom_vline(xintercept = as.numeric(division_of_waves1), col = "red", linetype = "dashed")+     ###### divis?es entre 1-2 ondas
  geom_vline(xintercept = as.numeric(ymd("2021-12-13")), col = "red", linetype = "dashed")+      ###### divis?es entre 2-3 ondas
  geom_vline(xintercept = as.numeric(ymd("2021-11-18")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2022-02-25")), col = "purple", linetype = "twodash")+
  
  annotate(geom = "text", x = as.Date("2020-03-08"), y = 50, size = 2, label = "Início da \n 1a onda")+
  annotate(geom = "text", x = as.Date(division_of_waves1), y = 50, size = 2, label = "Fim da \n 1a onda")+
  annotate(geom = "text", x = as.Date("2021-12-13"), y = 50, size = 2, label = "Fim da \n 2a onda")

##### GR?FICO CEAR? #####
graficoce <- dadosestado %>%
  ggplot(aes(x = date, y = rollmeannewCases_per_100k_inhabitants)) +
  geom_line(show.legend = F, color = "#00B6EB") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, size = 6))+
  ggtitle("FORTALEZA")+
  xlab("data") +
  ylab(' ')+
  scale_x_date(limits = c(ymd("2020-02-25"), ymd("2023-03-18")), date_breaks = "1 month", date_labels = "%b %y", date_minor_breaks = "6 months")+
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), col = "gray") +    ###### divisores dos semestres
  geom_vline(xintercept = as.numeric(ymd("2020-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-07-01")), col = "gray") +
  
  geom_vline(xintercept = as.numeric(ymd("2020-03-22")), col = "red", linetype = "dashed")+      ###### Início da primeira onda
  geom_vline(xintercept = as.numeric(division_of_waves1), col = "red", linetype = "dashed")+     ###### divis?es entre ondas
  geom_vline(xintercept = as.numeric(ymd("2021-09-23")), col = "red", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(ymd("2021-11-12")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2022-01-18")), col = "purple", linetype = "twodash")+
  
  annotate(geom = "text", x = as.Date("2020-03-22"), y = 175, size = 2, label = "Início da \n 1a onda")+
  annotate(geom = "text", x = as.Date(division_of_waves1), y = 175, size = 2, label = "Fim da \n 1a onda")+
  annotate(geom = "text", x = as.Date("2021-09-23"), y = 175, size = 2, label = "Fim da \n 2a onda")

##### GR?FICO GOI?S #####
graficogo <- dadosestado %>%
  ggplot(aes(x = date, y = rollmeannewCases_per_100k_inhabitants)) +
  geom_line(show.legend = F, color = "#00B6EB") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, size = 6))+
  ggtitle("GOIÂNIA")+
  xlab("data") +
  ylab(' ')+
  scale_x_date(limits = c(ymd("2020-02-25"), ymd("2023-03-18")), date_breaks = "1 month", date_labels = "%b %y", date_minor_breaks = "6 months")+
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), col = "gray") +    ###### divisores dos semestres
  geom_vline(xintercept = as.numeric(ymd("2020-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-07-01")), col = "gray") +
  
  geom_vline(xintercept = as.numeric(ymd("2020-03-15")), col = "red", linetype = "dashed")+      ###### Início da primeira onda
  geom_vline(xintercept = as.numeric(division_of_waves1), col = "red", linetype = "dashed")+     ###### divis?es entre ondas
  geom_vline(xintercept = as.numeric(ymd("2021-12-14")), col = "red", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(ymd("2021-12-09")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2022-02-25")), col = "purple", linetype = "twodash")+
  
  annotate(geom = "text", x = as.Date("2020-03-15"), y = 87.5, size = 2, label = "Início da \n 1a onda")+
  annotate(geom = "text", x = as.Date(division_of_waves1), y = 87.5, size = 2, label = "Fim da \n 1a onda")+
  annotate(geom = "text", x = as.Date("2021-12-14"), y = 87.5, size = 2, label = "Fim da \n 2a onda")

##### GR?FICO PAR? #####
graficopa <- dadosestado %>%
  ggplot(aes(x = date, y = rollmeannewCases_per_100k_inhabitants)) +
  geom_line(show.legend = F, color = "#00B6EB") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, size = 6))+
  ggtitle("BELÉM")+
  xlab("data") +
  ylab(' ')+
  scale_x_date(limits = c(ymd("2020-02-25"), ymd("2023-03-18")), date_breaks = "1 month", date_labels = "%b %y", date_minor_breaks = "6 months")+
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), col = "gray") +    ###### divisores dos semestres
  geom_vline(xintercept = as.numeric(ymd("2020-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-07-01")), col = "gray") +
  
  geom_vline(xintercept = as.numeric(ymd("2020-03-22")), col = "red", linetype = "dashed")+      ###### Início da primeira onda
  geom_vline(xintercept = as.numeric("2020-10-25"), col = "red", linetype = "dashed")+     ###### divis?es entre ondas
  geom_vline(xintercept = as.numeric(ymd("2021-10-30")), col = "red", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(ymd("2021-12-02")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2022-02-25")), col = "purple", linetype = "twodash")+
  annotate(geom = "text", x = as.Date("2020-03-22"), y = 33, size = 2, label = "Início da \n 1a onda")+
  annotate(geom = "text", x = as.Date("2020-10-25"), y = 33, size = 2, label = "Fim da \n 1a onda")+
  annotate(geom = "text", x = as.Date("2021-10-30"), y = 33, size = 2, label = "Fim da \n 2a onda")

##### GR?FICO PERNAMBUCO #####
graficope <- dadosestado %>%
  ggplot(aes(x = date, y = rollmeannewCases_per_100k_inhabitants)) +
  geom_line(show.legend = F, color = "#00B6EB") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, size = 6))+
  ggtitle("RECIFE")+
  xlab("data") +
  ylab(' ')+
  scale_x_date(limits = c(ymd("2020-02-25"), ymd("2023-03-18")), date_breaks = "1 month", date_labels = "%b %y", date_minor_breaks = "6 months")+
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), col = "gray") +    ###### divisores dos semestres
  geom_vline(xintercept = as.numeric(ymd("2020-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-07-01")), col = "gray") +
  
  geom_vline(xintercept = as.numeric(ymd("2020-03-15")), col = "red", linetype = "dashed")+      ###### Início da primeira onda
  geom_vline(xintercept = as.numeric(division_of_waves1), col = "red", linetype = "dashed")+     ###### divis?es entre ondas
  geom_vline(xintercept = as.numeric(ymd("2021-12-21")), col = "red", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(ymd("2021-11-18")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2022-02-25")), col = "purple", linetype = "twodash")+
  
  annotate(geom = "text", x = as.Date("2020-03-15"), y = 60, size = 2, label = "Início da \n 1a onda")+
  annotate(geom = "text", x = as.Date(division_of_waves1), y = 60, size = 2, label = "Fim da \n 1a onda")+
  annotate(geom = "text", x = as.Date("2021-12-21"), y = 60, size = 2, label = "Fim da \n 2a onda")

##### GR?FICO RIO GRANDE DO SUL #####
graficors <- dadosestado %>%
  ggplot(aes(x = date, y = rollmeannewCases_per_100k_inhabitants)) +
  geom_line(show.legend = F, color = "#00B6EB") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, size = 6))+
  ggtitle('RIO GRANDE DO SUL')+
  xlab("data") +
  ylab(' ')+
  scale_x_date(limits = c(ymd("2020-02-25"), ymd("2023-03-18")), date_breaks = "1 month", date_labels = "%b %y", date_minor_breaks = "6 months")+
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), col = "gray") +    ###### divisores dos semestres
  geom_vline(xintercept = as.numeric(ymd("2020-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-07-01")), col = "gray") +
  
  geom_vline(xintercept = as.numeric(ymd("2020-03-15")), col = "red", linetype = "dashed")+      ###### Início da primeira onda
  geom_vline(xintercept = as.numeric(division_of_waves1), col = "red", linetype = "dashed")+     ###### divis?es entre ondas
  geom_vline(xintercept = as.numeric(ymd("2021-12-23")), col = "red", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(ymd("2021-12-03")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2022-02-24")), col = "purple", linetype = "twodash")+
  
  annotate(geom = "text", x = as.Date("2020-03-15"), y = 150, size = 2, label = "Início da \n 1a onda")+
  annotate(geom = "text", x = as.Date(division_of_waves1), y = 150, size = 2, label = "Fim da \n 1a onda")+
  annotate(geom = "text", x = as.Date("2021-12-23"), y = 150, size = 2, label = "Fim da \n 2a onda")

##### GR?FICO S?O PAULO #####
graficosp <- dadosestado %>%
  ggplot(aes(x = date, y = rollmeannewCases_per_100k_inhabitants)) +
  geom_line(show.legend = F, color = "#00B6EB") +
  theme_light()+
  theme(axis.text.x = element_text(angle = 45, size = 6))+
  ggtitle("SÃO PAULO")+
  xlab("data") +
  ylab(' ')+
  scale_x_date(limits = c(ymd("2020-02-25"), ymd("2023-03-18")), date_breaks = "1 month", date_labels = "%b %y", date_minor_breaks = "6 months")+
  geom_vline(xintercept = as.numeric(ymd("2020-01-01")), col = "gray") +    ###### divisores dos semestres
  geom_vline(xintercept = as.numeric(ymd("2020-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2021-07-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-01-01")), col = "gray") +
  geom_vline(xintercept = as.numeric(ymd("2022-07-01")), col = "gray") +
  
  geom_vline(xintercept = as.numeric(ymd("2020-03-01")), col = "red", linetype = "dashed")+      ###### Início da primeira onda
  geom_vline(xintercept = as.numeric(division_of_waves1), col = "red", linetype = "dashed")+     ###### divis?es entre ondas
  geom_vline(xintercept = as.numeric(ymd("2021-12-22")), col = "red", linetype = "dashed")+
  geom_vline(xintercept = as.numeric(ymd("2021-11-24")), col = "purple", linetype = "twodash")+      ###### datas das coletas
  geom_vline(xintercept = as.numeric(ymd("2022-02-23")), col = "purple", linetype = "twodash")+
  
  annotate(geom = "text", x = as.Date("2020-03-01"), y = 40, size = 2, label = "Início da \n 1a onda")+
  annotate(geom = "text", x = as.Date(division_of_waves1), y = 40, size = 2, label = "Fim da \n 1a onda")+
  annotate(geom = "text", x = as.Date("2021-12-22"), y = 40, size = 2, label = "Fim da \n 2a onda")

###### IMPRIMINDO OS GR?FICOS ######
ggsave(filename = 'output/graficoba2.png', plot = graficoba, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficoce2.png', plot = graficoce, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficogo2.png', plot = graficogo, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficopa2.png', plot = graficopa, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficope2.png', plot = graficope, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficors2.png', plot = graficors, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficosp2.png', plot = graficosp, width = 29.7, height = 21/3.5, units=c('cm'))

ggsave(filename = 'output/graficosalvador.png', plot = graficoba, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficofortaleza.png', plot = graficoce, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficogoiania.png', plot = graficogo, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficobelem.png', plot = graficopa, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficorecife.png', plot = graficope, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficopalegre.png', plot = graficors, width = 29.7, height = 21/3.5, units=c('cm'))
ggsave(filename = 'output/graficospaulo.png', plot = graficosp, width = 29.7, height = 21/3.5, units=c('cm'))

ggsave(filename = 'output/graficobrasil2.png', plot = graficobrasil, width = 29.7, height = 21/3.5, units=c('cm'))
