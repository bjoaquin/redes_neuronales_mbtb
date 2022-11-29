library(VGAM)
library(ROCit)


# Carga de datos de entrenamiento + validacion

df <- read.csv('1_train.csv')



# Conversion de variables

df$station_num <- as.factor(df$station_num)

df$status <- as.factor(df$status)

df$mday <- as.factor(df$mday)

df$wday <- as.factor(df$wday)

df$month <- as.factor(df$month)

df$natural <- as.numeric(df$natural == "True")



# Ecuacion a ajustar

formula <- natural ~ time + blocked_anchors + station_num +
                      status + mday + wday + month



# Ajuste del modelo

modelo <- glm(formula, data=df, family=binomial(link="logit")) 



# Curva ROC

ROCit_obj <- rocit(score=modelo$fitted.values, class=df$natural)



# Seleccion de punto de corte (criterio Youden)

cutoff <- ROCit_obj$Cutoff[which.max(ROCit_obj$TPR - ROCit_obj$FPR)]



# Carga de datos de evaluacion

df.test <- read.csv('1_test.csv')



# Conversion de variables

df.test$station_num <- as.factor(df.test$station_num)

df.test$status <- as.factor(df.test$status)

df.test$mday <- as.factor(df.test$mday)

df.test$wday <- as.factor(df.test$wday)

df.test$month <- as.factor(df.test$month)

df.test$natural <- as.numeric(df.test$natural == "True")



# Matriz de variables predictoras

modMatrix <- model.matrix(formula, data=df.test)

# (Agrego manualmente las siguientes columnas porque el test dataset 
# no contiene registros pertenecientes a las estaciones 68 y 70)

station_num68 <- numeric(nrow(df.test))

station_num70 <- numeric(nrow(df.test))

modMatrix <- cbind(modMatrix[,1:69], station_num68, modMatrix[,70], 
                   station_num70, modMatrix[,71:ncol(modMatrix)])



# Respuestas observadas para el dataset de evaluacion

y_obs <- df.test$natural



# Respuestas predichas para el dataset de evaluacion

y_pred <- modMatrix %*% modelo$coefficients

y_pred <- as.numeric(y_pred)

y_pred <- 1/(1+exp((-1)*y_pred))

y_pred <- as.numeric(y_pred > cutoff)



# Precision

accuracy <- mean(y_obs == y_pred); print(accuracy)



# Valores para construir matriz de confusion

TP <- sum(y_obs==1 & y_pred ==1)

TN <- sum(y_obs==0 & y_pred ==0)

FP <- sum(y_obs==0 & y_pred ==1)

FN <- sum(y_obs==1 & y_pred ==0)

TP; FP; TN; FN



# A continuacion se prueban 4 nuevos modelos generados a partir de
# transformaciones de la variable time: cuadratica, raiz cuadrada,
# logaritmica y exponencial. Se incluyen algunas interacciones.


# Ecuaciones para los nuevos modelos

formula.quad <- natural ~ 
                      time + I(time^2) +
                      blocked_anchors + station_num +
                      status + mday + wday + month + 
                      time*wday + time*month + I(time^2)*wday + I(time^2)*month

formula.sqrt <- natural ~
                      sqrt(time) +
                      blocked_anchors + station_num +
                      status + mday + wday + month + 
                      sqrt(time)*wday + sqrt(time)*month

formula.log  <- natural ~
                      log(time) +
                      blocked_anchors + station_num +
                      status + mday + wday + month + 
                      log(time)*wday + log(time)*month

formula.exp  <- natural ~
                      exp(time) +
                      blocked_anchors + station_num +
                      status + mday + wday + month + 
                      exp(time)*wday + exp(time)*month



# Ajuste de los nuevos modelos

formulas <- c(formula.quad, formula.sqrt, formula.log, formula.exp)

formulas_names <- c("Quad", "Sqrt", "Log", "Exp")

accuracies <- NULL

for(formula in formulas){

  modelo2 <- glm(formula, data=df, family=binomial(link="logit")) 
  ROCit2 <- rocit(score=modelo2$fitted.values, class=df$natural)
  cutoff2 <- ROCit2$Cutoff[which.max(ROCit2$TPR - ROCit2$FPR)]
  
  modMatrix2 <- model.matrix(formula, data=df.test)
  modMatrix2 <- cbind(modMatrix2[,1:69], 
                      station_num68, 
                      modMatrix2[,70], 
                      station_num70, 
                      modMatrix2[,71:ncol(modMatrix2)])
  
  y_obs <- df.test$natural
  
  y_pred <- modMatrix2 %*% modelo2$coefficients
  y_pred <- as.numeric(y_pred)
  y_pred <- 1/(1+exp((-1)*y_pred))
  y_pred <- as.numeric(y_pred > cutoff)
  
  accuracy <- mean(y_obs == y_pred); print(accuracy)
  accuracies <- c(accuracies, accuracy)
}

print(paste0("Mejor modelo:", formulas_names[which.max(accuracies)], sep=" "))

print(paste0("Precision:", max(accuracies), sep=" "))



# Valores para construir matriz de confusion

TP <- sum(y_obs==1 & y_pred ==1)

TN <- sum(y_obs==0 & y_pred ==0)

FP <- sum(y_obs==0 & y_pred ==1)

FN <- sum(y_obs==1 & y_pred ==0)

TP; FP; TN; FN

