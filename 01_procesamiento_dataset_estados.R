library(dplyr)
library(tidyverse)
library(lubridate)
library(tigerstats)
library(philentropy)


data.estados <- read.csv('mbtb_estadocarga2021.csv')



# Remover registros de estaciones de prueba

data.estados <- data.estados[data.estados$id_station != 86,]

data.estados <- data.estados[data.estados$station_num != 86,]



# Corregir huso horario (de GMT+0 a GMT-3)

data.estados$created_at <- ymd_hms(data.estados$created_at)

hour(data.estados$created_at) <- hour(data.estados$created_at) - 3

data.estados$updated_at <- ymd_hms(data.estados$updated_at)

hour(data.estados$updated_at) <- hour(data.estados$updated_at) - 3



# Remover registros de viajes previos a la inauguracion de la estacion

data.estados <- data.estados[(data.estados$id_station != 55) | 
                               (data.estados$created_at > "2021-05-20"),]



# Eliminar variable redundante

data.estados <- data.estados[,
                   !(names(data.estados) %in% c("incidence_end_date"))
]



# Exportar dataset

write.csv(data.estados, 'estados_dep.csv', row.names = F)

