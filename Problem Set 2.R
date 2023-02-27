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
