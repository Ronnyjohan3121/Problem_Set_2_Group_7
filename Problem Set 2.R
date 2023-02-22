# Problem Set 2 Big Data and Machine Learning - Applied Economics 

# Update March 22 - 2023

# Students Catalina Esmeral , Juan Camilo Sanchez , Federico R , Ronny Johan Cruz

# Lo primero que realizamos es la limpieza del ambiente

rm(list = ls())

# From the recommendations given in the asynchronous class we must load the relevant libraries

require(pacman)

# We use pload so that we can have the libraries that we will need for the development of the code loaded and can be called

p_load(tidyverse)
p_load(rio) 
p_load(tidymodels)
p_loads(ggplot2)
p_loads(scales)
p_loads(skimr, 
        caret,
        r.vest,
        r.stringr,
        dplyr)

