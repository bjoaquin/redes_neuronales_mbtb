library(tidyverse)
library(lubridate)
library(tigerstats)
library(philentropy)


data.viajes <- read.csv('mbtb_viajes2021.csv')



# Remover registros de usuarios c/ edades extremas

data.viajes <- data.viajes[data.viajes$age %in% seq(12,90,1),]



# Remover registros de viajes de duracion extrema

data.viajes <- data.viajes[(data.viajes$time_minutes > 5) & 
                             (data.viajes$time_minutes < 60),]



# Remover registros de estaciones de prueba

data.viajes <- data.viajes[data.viajes$origin_station_id != 86,]



# Remover registros de viajes con cierre de tipo manual

data.viajes <- data.viajes[data.viajes$close_type != "Manual",]



# Corregir huso horario (de GMT+0 a GMT-3)

data.viajes$origin_dt <- ymd_hms(paste0(data.viajes$origin_date, " ",
                                        data.viajes$origin_time))

data.viajes$destination_dt <- ymd_hms(paste0(data.viajes$destination_date, " ",
                                             data.viajes$destination_time))

hour(data.viajes$origin_dt) <- hour(data.viajes$origin_dt) - 3

hour(data.viajes$destination_dt) <- hour(data.viajes$destination_dt) - 3

data.viajes$origin_date <- substr(data.viajes$origin_dt, start=1, stop=10)



# Remover viajes del 2020 (consecuencia del desfase en huso horario)

data.viajes <- data.viajes[data.viajes$origin_date >= "2021-01-01",]



# Remover registros de viajes previos a la inauguracion de la estacion

data.viajes <- data.viajes[(data.viajes$origin_station_num != 55) | 
                             (data.viajes$origin_date > "2021-05-20"),]

data.viajes <- data.viajes[(data.viajes$destination_station_num != 55) | 
                             (data.viajes$origin_date > "2021-05-20"),]



# Dia de la semana de origen de los viajes

wdays <- c('Domingo', 'Lunes', 'Martes', 'Miercoles',
           'Jueves', 'Viernes', 'Sabado')

data.viajes$origin_date_name <- wday(data.viajes$origin_date)

data.viajes$origin_date_name <- wdays[as.numeric(data.viajes$origin_date_name)]

rm(wdays)



# Mes de origen de los viajes

meses <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 'Junio', 'Julio',
           'Agosto', 'Septiembre', 'Octubre', 'Noviembre', 'Diciembre')

data.viajes$origin_month <- substr(data.viajes$origin_date, start=6, stop=7)

data.viajes$origin_month_name <- meses[as.numeric(data.viajes$origin_month)]

rm(meses)



# Descarto las variables auxiliares creadas anteriormente

data.viajes <- data.viajes[,!(names(data.viajes) %in% c("origin_dt"))]

data.viajes <- data.viajes[,!(names(data.viajes) %in% c("destination_dt"))]



# Version continua de las horas de inicio y fin de los viajes

data.viajes$origin_time <- 
  hour(data.viajes$origin_dt) + 
  minute(data.viajes$origin_dt) / 60 + 
  second(data.viajes$origin_dt) / 3600

data.viajes$destination_time <- 
  hour(data.viajes$destination_dt) + 
  minute(data.viajes$destination_dt) / 60 + 
  second(data.viajes$destination_dt) / 3600



# Exportar dataset

write.csv(data.viajes, 'viajes_dep.csv', row.names = F)

