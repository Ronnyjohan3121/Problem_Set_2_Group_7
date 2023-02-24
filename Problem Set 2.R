# Problem Set 2 Big Data and Machine Learning - Applied Economics 

# Update March 22 - 2023

# Students Catalina Esmeral , Juan Camilo Sanchez , Federico R , Ronny Johan Cruz

# The first thing we do is clean the environment

rm(list = ls())

# From the recommendations given in the asynchronous class we must load the relevant libraries

require(pacman)

# We use pload so that we can have the libraries that we will need for the development of the code loaded and can be called

p_load(tidyverse)
p_load(rio) 
p_load(tidymodels)
p_load(ggplot2)
p_load(scales)
p_load(skimr, 
        caret,
        r.vest,
        r.stringr,
        dplyr)

# We will read the data from our directory location 

test_hogares<-read.csv(file = 'test_hogares.csv')
test_personas<-read.csv(file = 'test_personas.csv')
train_hogares<-read.csv(file = 'train_hogares.csv')
train_personsas<-read.csv(file = 'train_personas.csv')

test<-merge(test_hogares,test_personas, by="id")
train<-left_join(train_hogares,train_personsas, by= "id")

# With the dictionary that the KAGGLE DANE WEB SITE cites, we look at the meaning of the variables, we proceed to carry out the mutation to go from categorical to one of type of factor

test<-test%>%
  mutate_at(.vars = c("Clase.x", "Dominio.x","P5090","Depto.x", "Clase.y", "Dominio.y",
                      "P6020","P6050", "P6090", "P6100","P6210", "P6240",
                      "Oficio","P6430", "P6510","P6545", "P6580","P6585s1", "P6585s2",
                      "P6585s3", "P6585s4", "P6590","P6600", "P6610", "P6620", "P6630s1",
                      "P6630s4","P6630s2","P6630s3","P6870", "P6920", "P7040","P7050",
                      "P7090", "P7110","P7120", "P7150","P7160", "P7310",
                      "P7150","P7350","P7422","P7472","P7495","P7500s2","P7500s3",
                      "P7505","P7510s1","P7510s2","P7510s3", "P7510s5","P7510s6","P7510s7",
                      "Pet", "Oc", "Des", "Ina", "Depto.y" ),.funs = factor)

train<-train%>%
  mutate_at(.vars = c("Clase.x", "Dominio.x","P5090", "Pobre", "Indigente","Depto.x",
                      "Clase.y", "Dominio.y", "Estrato1", "P6020", "P6050", "P6090",
                      "P6100", "P6210", "P6240", "Oficio","P6430", "P6510", "P6510s2",
                      "P6545", "P6545s2", "P6580","P6580s2", "P6585s1", "P6585s1a2",
                      "P6585s2", "P6585s2a2", "P6585s3", "P6585s3a2", "P6585s4",
                      "P6585s4a2", "P6590","P6600", "P6610", "P6620", "P6630s1",
                      "P6630s4","P6630s2", "P6630s3","P6870", "P6920", "P7040", "P7050",
                      "P7090", "P7110", "P7120", "P7140s1","P7140s2","P7150","P7160",
                      "P7310","P7150","P7350","P7422","P7472","P7495","P7500s1",
                      "P7500s2","P7500s3", "P7505","P7510s1","P7510s2","P7510s3",
                      "P7510s5","P7510s6","P7510s7","Pet", "Oc", "Des", "Ina", "Depto.y" ),.funs = factor)

summary(train_personsas$P7510s5)

summary(train$P7510s5)

# Now let´s star with data cleaning to accurately use the data

# We must exclude the NA´s within our TRAIN database

cantidad_na <- sapply(train, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
cantidad_na <- data.frame(cantidad_na) %>%
  rownames_to_column("variable")
cantidad_na$porcentaje_na <- cantidad_na$cantidad_na/nrow(train) # We generate the number of NA´s and the percentage that emans over the variable

# We eliminate the variables that can´t contribuite to our statistics

filtro <- cantidad_na$porcentaje_na > 0.85 # An appropriate amount must be greater than 85%
variables_eliminar <- cantidad_na$variable[filtro]
train <- train %>% 
  select(-variables_eliminar) # We must take into account minus sing

x <- ls(train)
y <- ls(test)
igualdad <- list()

for(i in 1:length(y)){ # vector length x
  for(j in 1:length(x)){ # vector length y
    if (y[i]==x[j]){
      igualdad <- append(igualdad, y[i])
    }
  }
}
igualdad
