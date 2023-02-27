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
