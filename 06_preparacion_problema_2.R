library(tidyverse)
library(tigerstats)
library(philentropy)


data.viajes <- read.csv('viajes_dep.csv')

data.estaciones <- read.csv('mbtb_estaciones2021.csv')



# Creacion de zonas (clusters) para las estaciones

set.seed(22)

clust <- hclust(dist(data.estaciones[,c('lat','lon')]), method="average")

data.estaciones$zona <- cutree(clust, 8)

rm(clust)



# Retener solo las variables de interes

data.estaciones <- data.estaciones[,(names(data.estaciones) %in% 
                                       c("station_code", "zona"))]


# Renombrar variables para que coincidan con dataset VIAJES

names(data.estaciones) <- c("destination_station_num", "destination_zone")



# Creacion de dataset auxiliar USUARIOS
# Calcula, para c/ usuario: 
#     1) Numero de viajes realizados
#     2) Proporcion de viajes finalizados en c/ estacion

user_id <- sort(unique(data.viajes$user_id))

data.usuarios <- cbind(
  user_id,
  as.data.frame.matrix(table(
    data.viajes$user_id, 
    data.viajes$destination_station_num)
  )
)

rm(user_id)

data.usuarios$n_viajes <- rowSums(data.usuarios[,2:73])

data.usuarios[2:73] <- data.usuarios[,2:73] / data.usuarios$n_viajes

colnames(data.usuarios) <- c("user_id", paste("s",1:72,sep=""), "n_viajes")



# Ordenar datasets VIAJES y USUARIOS por user_id

data.viajes <- data.viajes[order(data.viajes$user_id),]

data.usuarios <- data.usuarios[order(data.usuarios$user_id),]



# Unir datasets VIAJES y USUARIOS

data.viajes <- right_join(data.usuarios, data.viajes, by = "user_id")



# Unir datasets VIAJES y ESTACIONES

data.viajes <- right_join(data.estaciones, data.viajes, 
                          by = "destination_station_num")



# Retener solo las variables de interes

data.viajes <- data.viajes[,
   names(data.viajes) %in% 
     c(c("destination_zone", "n_viajes", "origin_station_num",
         "origin_month", "origin_date_name", "origin_time"),
       paste0("s",1:72,sep=""))
]



# Particion de dataset (training+valid, test)

set.seed(28)

data.viajes <- data.viajes[sample(1:nrow(data.viajes)),]

viajes.train  <- data.viajes[1:1000000,]

viajes.test   <- data.viajes[1000001:nrow(data.viajes),]



# Exportar datasets

write.csv(viajes.train, '2_train.csv', row.names = F)

write.csv(viajes.test, '2_test.csv', row.names = F)

write.csv(data.usuarios, 'usuarios_dep.csv', row.names = F)

