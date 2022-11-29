library(dplyr)

# Modelo naive: predice que cada usuario ira a su estacion mas frecuente


# Carga de datos

df.train <- read.csv('2_train.csv')

df.test  <- read.csv('2_test.csv')

df <- rbind(df.train, df.test)

rm(df.train, df.test)

df.estaciones <- read.csv('estaciones_dep.csv')



# Frecuencia de destino en c/zona, para c/ usuario

clusters <- df.estaciones$zona

for(i in 1:8){

    df[paste0('z',i, sep='')] <-
    rowSums(
      df[,paste0('s', which(clusters==i), sep='')]
    )
}

tmp <- df[,paste0('z', 1:8, sep='')] %>%
          rowwise %>%
          mutate( # En caso de empates entre zonas se elige la primera
            y_pred = names(.)[which.max(c(z1, z2, z3, z4, z5, z6, z7, z8))]
          ) %>%
          ungroup

tmp$y_pred <- as.numeric(substr(tmp$y_pred, 3, 3))

df$y_pred <- tmp$y_pred

rm(tmp)



# Precision (dataset de evaluacion)

df.test <- df[1000001:nrow(df),]

mean(df.test$destination_group == df.test$y_pred)





# Lo que sigue es el codigo con las pruebas que hice para concluir
# que no era viable ajustar un MLG bajo las condiciones del problema.
# Se puede correr pero puede ser lento y no se usa en la tesina.

library(VGAM)
library(mlogit)

df <- read.csv('infoextra_clasif_test.csv')
df <- df[sample(1:nrow(df), size=10000),]

df$origin_date_name <- as.factor(df$origin_date_name)
df$origin_month <- as.factor(df$origin_month)
df$origin_station_num <- as.factor(df$origin_station_num)

modelo <- vglm(destination_group ~ origin_time + n_viajes +
                 origin_date_name + origin_month, 
               data=df, family=multinomial)

summary(modelo)

df.mlogit <- mlogit.data(df, choice = "destination_group", shape = "wide")

mlogit(destination_group ~ 1 | origin_time + n_viajes +
         origin_date_name + origin_month,
       data = df.mlogit, reflevel = 8)
