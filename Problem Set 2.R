p_load(tidyverse,knitr,kableExtra,here,jtools,
       ggstance,broom,broom.mixed,skimr,caret,fastDummies,
       glmnet,MLmetrics,modelsummary,gamlr,class,AER,tidymodels,themis, stargazer, xtable)

setwd("C:/Users/juanc/OneDrive/Documents/PS2")

test_hogares<-read.csv(file = 'test_hogares.csv')
test_personas<-read.csv(file = 'test_personas.csv')
train_hogares<-read.csv(file = 'train_hogares.csv')
train_personas<-read.csv(file = 'train_personas.csv')


# Supongamos que quiero crear una variable que sea la suma de los ingresos de los individuos en el hogar a partir de la base de personas. Entonces:
sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)
# tengo entonces una base con id y la variable que acabo de crear Ingtot_hogar.

# Unir bases
# Puedo entonces unirla a la base de hogares. Para ello voy a usar la función left_join() de dplyr.
train_hogares<-left_join(train_hogares,sum_ingresos)
colnames(train_hogares)

# Tengo ahora una columna extra que es Ingtot_hogar
head(train_hogares[c("id","Ingtotug","Ingtot_hogar")])

# Cálculo de Pobreza
# Según la base del DANE un hogar es clasificado pobre si el “Ingreso percápita de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios” es menor a la Linea de pobreza que le corresponde al hogar.
table(train_hogares$Pobre) 
# no pobre 131936 y pobre 33024 

# Para testear si esto es cierto comparemos la variable Pobre incluida en la base con una creada por nosotros siguiendo el enfoque del DANE.
train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

# PREPARACIÓN DE LAS BASES DE DATOS

library(haven)

# variables que se van a utilizar:

# Pobre
# Clase
# P5010 - cantidad de cuartos de dormir (sirve para sacar cuantos duermen por cuarto)
# P5090 – vivienda propia
# Nper = número de personas
# Npersug = número de personas por unidad de gasto
# Intotug (hay que hacer un logintotug para segundo punto)
# Ingtotugarr
# Ingpcug
# Lp
# P6020 – sexo 
# P6040 – edad
# P6050 - jefe hogar
# P6100 - regimen salud
# P6210 – nivel educativo
# P6430 - posición en trabajo
# P6920 - cotiza pensión
# Oc – ocupado 
# P6240 - actividad

# dejar en las bases solamente las variables que voy a usar

test_hogares <- select(test_hogares, id, Clase, P5010, P5090, Nper, Npersug, Lp)
test_personas <- select(test_personas, id, Clase, P6020, P6040, P6050, P6100, P6210, P6430, P6920, Oc)
train_hogares <- select(train_hogares, id, Clase, P5010, P5090, Nper, Npersug, Ingtotug, Ingtotugarr, Ingpcug, Lp, Pobre, Ingtot_hogar)
train_personas <- select(train_personas, id, Clase, P6020, P6040, P6050, P6100, P6210, P6430, P6920, Oc, Ingtot)

# se crean las variables 

personas_h = train_hogares$Nper/train_hogares$P5010 # personas por habitación
train_hogares <- cbind(train_hogares, personas_h)

mujer_jh <- as.data.frame(ifelse((train_personas$P6020==1 & train_personas$P6050==1),1,0))
train_personas <- cbind(train_personas, mujer_jh)
mujer_jh <- train_personas %>% group_by(id) %>% summarize(mujer_jh=sum(mujer_jh,na.rm = TRUE)) 

edad_jh <- (ifelse((train_personas$P6050==1),train_personas$P6040,0))
train_personas<- cbind(train_personas, edad_jh)
edad_jh <-train_personas %>% group_by(id) %>% summarize(edad_jh=sum(edad_jh,na.rm = TRUE)) 

edu_jh <- (ifelse((train_personas$P6050==1),train_personas$P6210,0))
train_personas<- cbind(train_personas, edu_jh )
edu_jh  <-train_personas %>% group_by(id) %>% summarize(edu_jh = sum(edu_jh ,na.rm = TRUE)) 

salud_jh <- (ifelse((train_personas$P6050==1),train_personas$P6100,0))
train_personas <- cbind(train_personas, salud_jh)
salud_jh <- train_personas %>% group_by(id) %>% summarize(salud_jh=sum(salud_jh,na.rm = TRUE)) 

trabajo_ocu_jh <- (ifelse((train_personas$P6050==1),train_personas$P6430,0))
train_personas<- cbind(train_personas, trabajo_ocu_jh)
trabajo_ocu_jh <-train_personas %>% group_by(id) %>% summarize(trabajo_ocu_jh=sum(trabajo_ocu_jh,na.rm = TRUE)) 

pension_jh <- (ifelse((train_personas$P6050==1),train_personas$P6920,0))
train_personas<- cbind(train_personas, pension_jh)
pension_jh <-train_personas %>% group_by(id) %>% summarize(pension_jh =sum(pension_jh ,na.rm = TRUE)) 

ocu_jh <- (ifelse((train_personas$P6050==1),train_personas$Oc,0))
train_personas<- cbind(train_personas, ocu_jh)
ocu_jh <-train_personas %>% group_by(id) %>% summarize(ocu_jh =sum(ocu_jh ,na.rm = TRUE)) 

# ahora se pegan las variables creadas a la base train_hogares
library(dplyr)

train_hogares<-left_join(train_hogares, mujer_jh)
train_hogares<-left_join(train_hogares, edad_jh)
train_hogares<-left_join(train_hogares, edu_jh)
train_hogares<-left_join(train_hogares, salud_jh)
train_hogares<-left_join(train_hogares, trabajo_ocu_jh)
train_hogares<-left_join(train_hogares, pension_jh)
train_hogares<-left_join(train_hogares, ocu_jh)

# ya me quedaron todas las variables en train hogares
# ahora creo base train_hogares2 (se trabajará sobre esta)

train_hogares2 <- train_hogares

# para las regresiones voy as trabajar con las variables: Pobre ~ Clase + personas_h + P5090 + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + trabajo_ocu_jh + pension_jh + ocu_jh
# se mira cuáles variables son factor y se convierten

View(train_hogares2)

# en base train_hogares2 las variables factor son: Clase, P5090, Pobre, mujer_jh, edu_jh, salud_jh, trabajo_ocu_jh, pension_jh, ocu_jh
# de ahí en adelante se trabaja con la base train hogares 2

train_hogares2 <- train_hogares2 %>%
  mutate_at(.vars = c("Clase", "P5090", "Pobre", "mujer_jh", "edu_jh", "trabajo_ocu_jh", "pension_jh", "ocu_jh", "salud_jh"),.funs = factor)

train_hogares2 <- train_hogares2 %>%
  mutate_at(.vars = c("salud_jh"),.funs = factor)

###############

# se siembra semilla y se parte la base train hogares 2 con nrow, en train y test

set.seed(1010) 
train_hogares2 <- train_hogares2 %>%
  mutate(holdout= as.logical(1:nrow(train_hogares2) %in%
                               sample(nrow(train_hogares2), nrow(train_hogares2)*.2))
  )
test<-train_hogares2[train_hogares2$holdout==T,] 
train<-train_hogares2[train_hogares2$holdout==F,] 

summary(test)
summary(train)

prop.table(table(train$Pobre))

# ver si hay missing values en las variables que voy a usar para los modelos:

sum(is.na(train$Pobre)) # 0 missing 
sum(is.na(train$Clase)) # 0 missing 
sum(is.na(train$personas_h)) # 0 missing 
sum(is.na(train$P5090)) # 0 missing 
sum(is.na(train$Npersug)) # 0 missing 
sum(is.na(train$Nper)) # 0 missing 
sum(is.na(train$mujer_jh)) # 0 missing 
sum(is.na(train$edad_jh)) # 0 missing 
sum(is.na(train$salud_jh)) # 0 missing 
sum(is.na(train$edu_jh)) # 0 missing 
sum(is.na(train$trabajo_ocu_jh)) # 0 missing 
sum(is.na(train$pension_jh)) # 0 missing 
sum(is.na(train$ocu_jh)) # 0 missing 



# Tablas de algunas de las variables que elegí

install.packages("gtsummary")
require ("gtsummary") 
require("haven")
train <- zap_labels(train)


#######

library(pacman)
p_load(GGally)
install.packages("GGally")
library(GGally)
#Estadísticas descriptivas


names(train) <- c("id", "Ubic", "n_habitaciones", "tipo_vivienda",
                  "Nper", "Npersug", "Ingtotug", "Ingtotugarr", "Ingcug", "Lp",
                  "Pobre", "Ingtot_hogar", "personas_h", "mujer_jh", "edad_jh", "edu_jh",
                  "salud_jh", "trabajo_ocu_jh", "pension_jh", "ocu_jh", "holdout")
names(test) <- c("id", "Ubic", "n_habitaciones", "tipo_vivienda",
                  "Nper", "Npersug", "Ingtotug", "Ingtotugarr", "Ingcug", "Lp",
                  "Pobre", "Ingtot_hogar", "personas_h", "mujer_jh", "edad_jh", "edu_jh",
                  "salud_jh", "trabajo_ocu_jh", "pension_jh", "ocu_jh", "holdout")

p_load(gridExtra)
library(gridExtra)
variables_categoricas <- names(train[, sapply(train, is.factor)])
#loop para gráficos múltiples
for (var in variables_categoricas) {
  p1<- ggplot(train, aes(x = Ingtotug, fill = .data[[var]])) +
    geom_density(alpha = 0.4) +
    labs(x = "Ingresos totales por hogar (COP)",
         y = "Densidad",
  title = paste("Distribución de ingresos por", var)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    coord_cartesian(xlim = c(0, 40000000))
  
  p2 <- ggplot(train, aes(y = .data[[var]])) +
    geom_bar(aes(x = (..count..)/sum(..count..)),
             fill = "darkblue") +
    labs(title = paste("Distribución de la variable", var),
         x = "Proporción (%)") +
    scale_x_continuous(labels = scales::percent) +
    theme_bw()
  
  grid.arrange(p1,p2, ncol =2)
}

library(stargazer)

stargazer(train[c("Nper", "Ingtotug", "total_female", "num_ocu")], type = "text")

#tablas

train %>%
  select(tipo_vivienda, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

train %>%
  select(Ubic, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

#####Modelo

library(tidyverse)

   


## modelos LOGIT
set.seed(1010)

logit2 <- glm(Pobre ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + pension_jh + ocu_jh, data = train, family = binomial)
logit3 <- glm(Pobre ~ Ubic + personas_h + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, data = train, family = binomial)
logit4 <- glm(Pobre ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, data = train, family = binomial)


summary(logit2, type = "text")
summary(logit3, type = "text")
summary(logit4, type = "text")

stargazer (logit2, logit3, logit4, type = "text")

#especificaciones 3 y 4 tienen el mejor ajuste

# PROBIT

# estimación probit

probit1 <- glm(Pobre ~ Ubic + personas_h + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, family=binomial(link="probit") , data=train)
probit1
summary(probit1)
stargazer(probit1, type="text")
tidy(probit1)

probit2 <- glm(Pobre ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, family=binomial(link="probit") , data=train)
probit2
summary(probit2)
stargazer(probit2, type="text")
tidy(probit2)

stargazer(logit1, logit2, logit3, logit4, probit1, probit2, type = "text")

## ratio de los coeficientes de los 2 mejores modelos
logit4$coefficients / probit2$coefficients

prop.table(table(train$Pobre))

# predicction 

library(dplyr)



test$pred_log2 <- predict(logit2, newdata=test, type="response")
test$pred_log3 <- predict(logit3, newdata=test, type="response")
test$pred_log4 <- predict(logit4, newdata=test, type="response")
test$pred_pro1 <- predict(probit1, newdata=test, type="response")
test$pred_pro2 <- predict(probit2, newdata=test, type="response")

head(test)


ggplot(data=test , mapping = aes(Pobre, pred_log2)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_log3)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_log4)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_pro1)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_pro2)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()


test <- test %>% 
  mutate(p_logit_2 = ifelse(pred_log2 < 0.23,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))

###

## definir la regla (0.5)

rule=0.5

test$phat2 = ifelse(test$pred_log2>rule,1,0)
test$phat3 = ifelse(test$pred_log3>rule,1,0)
test$phat4 = ifelse(test$pred_log4>rule,1,0)
test$phat5 = ifelse(test$pred_pro1>rule,1,0)
test$phat6 = ifelse(test$pred_pro2>rule,1,0)

head(test)
View(test)

## Clasificación (sale matriz de confusión)

## logit

cm_log2 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat2) , mode="sens_spec" , positive="1")
cm_log2

cm_log3 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat3) , mode="sens_spec" , positive="1")
cm_log3

cm_log4 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat4) , mode="sens_spec" , positive="1")
cm_log4

## probit
cm_pro1 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat5) , mode="sens_spec" , positive="1")
cm_pro1

cm_pro2 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat6) , mode="sens_spec" , positive="1")
cm_pro2

cm1 <- cm_log1$table # matriz de confusión del modelo logit 1
cm2 <- cm_log2$table # matriz de confusión del modelo logit 2
cm3 <- cm_log3$table # matriz de confusión del modelo logit 3
cm4 <- cm_log4$table # matriz de confusión del modelo logit 4
cm5 <- cm_pro1$table # matriz de confusión del modelo probit 1
cm6 <- cm_pro2$table # matriz de confusión del modelo probit 2

# métricas por modelo
cm1_metri <- cm_log1$byClass
cm2_metri <- cm_log2$byClass
cm3_metri <- cm_log3$byClass
cm4_metri <- cm_log4$byClass
cm5_metri <- cm_pro1$byClass
cm6_metri <- cm_pro1$byClass

cm2_metri
# métricas agruipadas
gru_metri <-  rbind(cm2_metri, cm3_metri, cm4_metri, cm5_metri, cm6_metri)

#logit 2 y probit 2

####

# se seleccionó el modelo logit3

require(caret)

# Para desbalance de clases - evitar overfitting

require("tidyverse")
require("here")

# Partir base de datos train_hogares2

set.seed(1010)
# training
split1 <- createDataPartition(train_hogares2$Pobre, p = .7)[[1]]
length(split1)
other <- train_hogares2[-split1,]
training <- train_hogares2[ split1,]
#  ahora se crea evaluation y testing
set.seed(1010)
split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]
evaluation <- other[ split2,]
testing <- other[-split2,]

dim(training)
dim(evaluation)
dim(testing)

summary(training$Pobre)

# método rpart sin necesidad de incluir alguna grilla

set.seed(1010)
cv5 <- trainControl(number = 5, method = "cv")
cv3 <- trainControl(number = 3, method = "cv")

modelo1 <- train(Pobre ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh,
                 data = train, 
                 method = "rpart", 
                 trControl = cv5)

library(rattle)
fancyRpartPlot(modelo1$finalModel)

y_hat_insample1 = predict(modelo1, newdata = train)
test$y_hat_arbol1 = predict(modelo1, newdata = test)


cm_arbol1 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$y_hat_arbol1) , mode="sens_spec" , positive="1")
cm_arbol1



p_load(ranger)
install.packages("ranger")
library(ranger)

p_load(randomForest)
#corremos un bosque 
forest1 <- train(Pobre ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh,
                data = train, 
                method = "rf",
                trControl = cv3,
                metric="Accuracy",
)

forest_in_sample = predict(forest1, newdata = train)
test$forest_out_sample = predict(forest1, newdata = test)

cm_forest1 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$forest_out_sample) , mode="sens_spec" , positive="1")
cm_forest1


#Trad GBM

p_load(gbm)


gbm_res <- train(Pobre ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh,
                 data = train, 
                 method = "gbm", 
                 trControl = cv3,
                 #  family = "binomial", 
                 metric = "Accuracy"
                 
                 
)            

gbm_res #muestra el mejor accuracy con 150 árboles

gbm_in_sample = predict(gbm_res, newdata = train)
test$gbm_out_sample = predict(gbm_res, newdata = test)


cm_gbm = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$gbm_out_sample) , mode="sens_spec" , positive="1")
cm_gbm

# MODELOS DE REGRESIÓN

library("dplyr") #for data wrangling
library("caret") #ML
set.seed(1010) #set the seed for replication purposes
str(train_hogares2) #conmpact display

ols <- train(Ingtotug ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, # model to fit
             data = train,
             trControl = trainControl(method = "cv", number = 10),
             # Method: crossvalidation, 10 folds
             method = "lm")
# specifying regression model

ols

lambda <- 10^seq(-2, 3, length = 100)
lasso <- train(
  Ingtotug ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda=lambda), preProcess = c("center", "scale")
)

lasso

ridge <- train(
  Ingtotug ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, data = train, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0,lambda=lambda), preProcess = c("center", "scale")
)
ridge

install.packages("leaps")
require("leaps")
best<-regsubsets(Ingtotug ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, method="exhaustive",data = train)
summary(best)


forward <- train(Ingtotug ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, data = train,
                 method = "leapForward",
                 trControl = trainControl(method = "cv", number = 10))
forward

summary(forward$finalModel)

backwards <- train(Ingtotug ~ Ubic + personas_h + tipo_vivienda + Npersug + edad_jh + salud_jh + edu_jh + ocu_jh, data = train,
                   method = "leapBackward",
                   trControl = trainControl(method = "cv", number = 10))
backwards


summary(backwards$finalModel)

