library(readxl)
dataset <- read_excel("~/LECO/PROJETO SVRI/Timeline legislação/indicadores_casos_obitos/indicadores_wcota_casos_obitos.xlsx")

library(ggplot2)
ggplot() + 
  geom_line(dataset, mapping = aes(date, newDeaths), color = 'red')

