install.packages("COVID19")
library(dplyr)
library(readr)
library(zoo)
require(COVID19)
brasil <- covid19(country = "Brazil", level = 3, gmr = T)
fortaleza <- brasil %>% filter(administrative_area_level_3 == "Fortaleza")
belem <- brasil %>% filter(administrative_area_level_3 == "Belém")
goiania <- brasil %>% filter(administrative_area_level_3 == "Goiânia")
portoalegre <- brasil %>% filter(administrative_area_level_3 == "Porto Alegre")
recife <- brasil %>% filter(administrative_area_level_3 == "Recife")
salvador <- brasil %>% filter(administrative_area_level_3 == "Salvador")
saopaulo <- brasil %>% filter(administrative_area_level_3 == "São Paulo")

brasil2 <- covid19(country = "Brazil", level = 2, gmr = T)
Ceará <- brasil2 %>% filter(administrative_area_level_2 == "Ceará")

##### ADICIONANDO NEW CASES #####
fortaleza <- fortaleza %>% 
  mutate(newCases = confirmed-lag(confirmed)) %>% 
  mutate(newCases_per_100k_inhabitants = (newCases/population)*100000) %>%
  mutate(rollmeannewCases = rollmean(newCases, 7, fill=NA)) %>% 
  mutate(rollmeannewCases_per_100k_inhabitants = rollmean(newCases_per_100k_inhabitants, 7, fill = NA))

belem <- belem %>% 
  mutate(newCases = confirmed-lag(confirmed)) %>% 
  mutate(newCases_per_100k_inhabitants = (newCases/population)*100000) %>% 
  mutate(rollmeannewCases = rollmean(newCases, 7, fill=NA)) %>% 
  mutate(rollmeannewCases_per_100k_inhabitants = rollmean(newCases_per_100k_inhabitants, 7, fill = NA))

goiania <- goiania %>% 
  mutate(newCases = confirmed-lag(confirmed)) %>% 
  mutate(newCases_per_100k_inhabitants = (newCases/population)*100000) %>% 
  mutate(rollmeannewCases = rollmean(newCases, 7, fill=NA)) %>% 
  mutate(rollmeannewCases_per_100k_inhabitants = rollmean(newCases_per_100k_inhabitants, 7, fill = NA))

portoalegre <- portoalegre %>% 
  mutate(newCases = confirmed-lag(confirmed)) %>% 
  mutate(newCases_per_100k_inhabitants = (newCases/population)*100000) %>% 
  mutate(rollmeannewCases = rollmean(newCases, 7, fill=NA)) %>% 
  mutate(rollmeannewCases_per_100k_inhabitants = rollmean(newCases_per_100k_inhabitants, 7, fill = NA))

recife <- recife %>% 
  mutate(newCases = confirmed-lag(confirmed)) %>% 
  mutate(newCases_per_100k_inhabitants = (newCases/population)*100000) %>% 
  mutate(rollmeannewCases = rollmean(newCases, 7, fill=NA)) %>% 
  mutate(rollmeannewCases_per_100k_inhabitants = rollmean(newCases_per_100k_inhabitants, 7, fill = NA))

salvador <- salvador %>% 
  mutate(newCases = confirmed-lag(confirmed)) %>% 
  mutate(newCases_per_100k_inhabitants = (newCases/population)*100000) %>% 
  mutate(rollmeannewCases = rollmean(newCases, 7, fill=NA)) %>% 
  mutate(rollmeannewCases_per_100k_inhabitants = rollmean(newCases_per_100k_inhabitants, 7, fill = NA))

saopaulo <- saopaulo %>% 
  mutate(newCases = confirmed-lag(confirmed)) %>% 
  mutate(newCases_per_100k_inhabitants = (newCases/population)*100000) %>% 
  mutate(rollmeannewCases = rollmean(newCases, 7, fill=NA)) %>% 
  mutate(rollmeannewCases_per_100k_inhabitants = rollmean(newCases_per_100k_inhabitants, 7, fill = NA))

casos_capitais <- bind_rows(fortaleza, belem, goiania, portoalegre, recife, salvador, saopaulo)

write_rds(casos_capitais, "casos_capitais.rds")
