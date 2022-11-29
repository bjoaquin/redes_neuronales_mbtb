library(tidyverse)
library(tigerstats)
library(lubridate)


data.viajes <- read.csv('viajes_dep.csv')

data.estados <- read.csv('estados_dep.csv')



# Remover registros correspondientes a estaciones 55 y 56

data.estados <- data.estados[order(data.estados$station_num),]

data.estados <- data.estados[!(data.estados$station_num %in% 55:56),]



# Encontrar los pares consecutivos de cambios nulos y...
# ...efectuar la correccion correspondiente

k <- 3.14

for(i in nrow(data.estados):2){
  
  if(data.estados$status[i-1] == data.estados$status[i]){
    
    if(i == k - 2 & data.estados$incidence_time[i+1] < 1/60){
      
      if(data.estados$status[i]==data.estados$status[i-1]){
        
        tmp <- data.estados[i,]
        data.estados[i,] <- data.estados[(i+1),]
        data.estados[(i+1),] <- tmp
      }
    }
    k <- i
  }
}



# Retener unicamente los casos de interes

estados.tmp <- data.estados[0,]

for(i in nrow(data.estados):2){
  
  if(data.estados$station_num[i]==data.estados$station_num[i-1] &&
                    data.estados$id_status_for_station[i] != 58 && 
                    data.estados$id_status_for_station[i-1] == 58){
     
      estados.tmp <- rbind(tmp, data.estados[i,])
  }
}

data.estados <- estados.tmp

rm(estados.tmp)



# Dataset auxiliar que incluye solo aquellos...
# ...estados resueltos por usuarios

data.estados$natural <- logical(nrow(data.estados))

count <- 0

for(i in 1:nrow(data.estados)){
  
  dt <- paste(data.estados$updated_at[i])
  d <- strsplit(dt, split=" ")[[1]][1]
  t <- strsplit(dt, split=" ")[[1]][2]
  dt <- ymd_hms(dt)
  
  status_cond <- data.estados$status[i] == "llena"
  
  if(status_cond) {
    cond <- data.viajes$origin_date == d &
            data.viajes$origin_station_num == data.estados$station_num[i]
  } else {
    cond <- data.viajes$destination_date == d &
            data.viajes$destination_station_num == data.estados$station_num[i]
  }
  
  viajes.tmp <- data.viajes[cond,]
  
  if(status_cond){
    viaje.dt <- ymd_hms(paste(viajes.tmp$origin_date,
                              viajes.tmp$origin_time))
  } else {
    viaje.dt <- ymd_hms(paste(viajes.tmp$destination_date,
                              viajes.tmp$destination_time))
  }
  
  s <- sum(viaje.dt < dt & viaje.dt > dt - minutes(1))
  
  count <- count + as.numeric(as.logical(s))
  
  if(s > 0) {data.estados$natural[i] <- TRUE}
}

rm(viajes.tmp)



# Buscar para c/ estacion el viaje mas temprano

min_dt <- ymd_hms()

for(s in unique(data.estados$station_num)){
  
  viajes.tmp <- data.viajes[data.viajes$origin_station_num == s | 
                            data.viajes$destination_station_num == s,]
  
  origin_dt <- ymd_hms(paste(viajes.tmp$origin_date, 
                             viajes.tmp$origin_time))
  
  dest_dt <- ymd_hms(paste(viajes.tmp$destination_date, 
                           viajes.tmp$destination_time))
  
  min_dt <- c(min_dt, min(c(min(origin_dt), min(dest_dt))))
}

rm(viajes.tmp)



# Eliminar registros de estados anteriores al primer viaje de su estacion

estados.nuevo <- data.estados[0,]

estaciones <- unique(data.estados$station_num)

for(s in 1:length(estaciones)){
  
  estados.tmp <- data.estados[data.estados$station_num == estaciones[s],]
  estados.nuevo <- rbind(estados.nuevo, 
                         estados.tmp[estados.tmp$created_at >= min_dt[s],])
}

data.estados <- estados.nuevo

rm(estaciones, estados.tmp, estados.nuevo)



# Creacion de variables auxiliares

data.estados$wday <- wday(data.estados$created_at)

data.estados$mday <- mday(data.estados$created_at)

data.estados$month <- month(data.estados$created_at)

data.estados$time <- hour(data.estados$created_at) + 
                      minute(data.estados$created_at) / 60 + 
                      second(data.estados$created_at) / 3600



# Retener solo las variables de interes

data.estados <- data.estados[,
                              names(data.estados) %in% 
                              c('station_num', 'status', 'wday', 'mday', 
                                'month', 'time', 'blocked_anchors', 
                                'incidence_time', 'natural')
]



# Particion de dataset (training+valid, test)

set.seed(27)

data.estados <- data.estados[sample(1:nrow(data.estados)),]

estados.train  <- data.estados[1:60000,]

estados.test   <- data.estados[60001:nrow(data.estados),]



# Exportar datasets

write.csv(estados.train, '1_train.csv', row.names = F)

write.csv(estados.test, '1_test.csv', row.names = F)

