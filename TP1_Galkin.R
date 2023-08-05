#Instalo las librerías pertinentes y procedo a abrir el archivo

library(tidyverse)

base <- read.csv(file ="Datos/sitios-de-wifi.csv", stringsAsFactors = TRUE, encoding="UTF-8")

#La BASE son los Puntos de Wi-Fi Públicos de CABA y ofrece información y ubicación geográfica de los mismos en plazas,
#parques, sedes comunales y bibliotecas de la Ciudad.

summary(base) # ahora procederemos a tener una imagen general de la BASE

#La información se organiza en la siguientes categorías: long, lat, id, nombre, tipo, etapa, etapa_obse, estado, 
#subcategoria, nombre y altura de la calle, barrio, comuna y codigo postal

#A su vez se observa que, la mayor cantidad de Puntos de Wi-Fi:
#Se ubica en espacios públicos, mientras que la menor en espacios de cultura.
#La ejecución ocurrió en la gestión previa a Horacio Rodriguez Larreta (HRL), seguida por la Segunda Etapa de HRL
#El barrio que mas Puntos de Wi-Fi tiene es San Nicolás mientras que el que menos tiene es Barracas.

#Reduzco la base para trabajar con la información que me importa

base2 <- base %>% 
  select(nombre,tipo,etapa,subcategor,barrio) %>%
  mutate(etapa = recode(etapa,"Primera etapa HRL"= "Gestión HRL","Segunda etapa HRL"= "Gestión HRL","Segunda Etapa HRL" = "Gestión HRL")) %>% #corrijo nombre de categorías
  filter(etapa!="") # elimino los Puntos Wi-Fi que no se corresponden con ninguna etapa

#Ahora quiero comparar la cantidad de Puntos Wí-Fi creados en el subte anstes y después de la gestión de HRL 
#y ver cual fue la línea más beneficiada en ambos casos

Gestion_HRL <- base2 %>%
  filter(tipo=="Subte"& etapa=="Gestión HRL") %>% 
  count(subcategor) %>%
  arrange(-n) %>%
  print ()

summarise(Gestion_HRL,sum(n)) 


Gestion_NHRL <- base2 %>%
  filter(tipo=="Subte"& etapa=="Gestión previa HRL") %>% 
  count(subcategor) %>%
  arrange(-n) %>%
  print ()

summarise(Gestion_NHRL,sum(n)) 

## Total de Puntos Wí-Fi creados en el Subte:

#Gestión HRL: 79    Gestion_NHRL:16

## 1  Linea A 16    Línea D 6
## 2  Línea B 16    Linea A 3
## 3  Linea D 16    Línea B 3
## 4  Línea E 14    Línea H 3
## 5  Línea C  9    Línea E 1
## 6  Línea H  8