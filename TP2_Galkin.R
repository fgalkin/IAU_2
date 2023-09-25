######### LIBRERÏAS #########

library(tidyverse)
library(geoAr)
library(geofacet)
library(sf)
library(gridExtra)


######### ARCHIVOS #########

base <- read.csv(file ="Datos/sitios-de-wifi.csv",
                 stringsAsFactors = TRUE, encoding="UTF-8")

comunas <- st_read("Datos/comunas/comunas_wgs84.shp",
                stringsAsFactors = TRUE, options = "UTF-8")

######### TP N° 1 #########

# La BASE son los Puntos de Wi-Fi Públicos de CABA y ofrece información y ubicación geográfica de los mismos en plazas,
# parques, sedes comunales y bibliotecas de la Ciudad.

summary(base) # ahora procederemos a tener una imagen general de la BASE

# La información se organiza en la siguientes categorías: long, lat, id, nombre, tipo, etapa, etapa_obse, estado, 
# subcategoria, nombre y altura de la calle, barrio, comuna y codigo postal

# A su vez se observa que, la mayor cantidad de Puntos de Wi-Fi:
# Se ubica en espacios públicos, mientras que la menor en espacios de cultura.
# La mayor ejecución ocurrió en la gestión previa a Horacio Rodriguez Larreta (HRL), seguida por la Segunda Etapa de HRL
# El barrio que mas Puntos de Wi-Fi tiene es San Nicolás mientras que el que menos tiene es Barracas.

# Reduzco la base para trabajar con la información que me importa

base2 <- base %>% 
  select(nombre,tipo,etapa,subcategor,barrio) %>%
  mutate(etapa = recode(etapa,"Primera etapa HRL"= "Gestión HRL","Segunda etapa HRL"= "Gestión HRL",
                        "Segunda Etapa HRL" = "Gestión HRL","Ejecutados 2017"="Gestión HRL")) %>% #corrijo nombre de categorías
  filter(etapa!="") # elimino los Puntos Wi-Fi que no se corresponden con ninguna etapa

# Ahora quiero comparar la cantidad de Puntos Wí-Fi creados en el subte antes y después de la gestión de HRL 
# y ver cual fue la línea más beneficiada en ambos casos

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


######### TP N° 2 #########

# Ahora procedo a analizar como se distribuyó la ejecución de los Puntos de Wi-Fi Públicos de CABA a o largo 
# de las comunas y entre las distintas gestiones. Para ello, empleare la librería "geoAr", que me permite permitirá
# visualizar los datos por comuna mediante una grilla que ordena las mismas según su disposición en el mapa.

show_arg_codes(viewer = T) # Exploro ID de grillas

caba <- get_grid (district = "CABA") # Descargo Grilla CABA
 
# Gestiono la base, para seleccionar lo que me interesa y prepararla para unirla con la grilla de columnas 

base3 <- base %>% 
  select(etapa,comuna) %>%
  mutate(etapa = recode(etapa,"Gestión previa HRL"="Gestión MM",
                        "Primera etapa HRL"= "1° Gestión HRL",
                        "Segunda etapa HRL"= "2° Gestión HRL",
                        "Segunda Etapa HRL" = "2° Gestión HRL",
                        "Ejecutados 2017"="2° Gestión HRL")) %>% # Corrijo nombre de categorías
  filter(etapa !="" & comuna != "") %>% # Elimino los Puntos Wi-Fi que no se corresponden con ninguna etapa o comuna
  group_by(comuna,etapa) %>% # Agrupo los Puntos Wi-Fi por comuna y etapa
  summarize(ptos_wifi = n()) %>%  # Cuento los Puntos Wi-Fi y los pongo en una nueva columna
  mutate(comuna = gsub("Comuna", "COMUNA", comuna), # corrijo los nombres de las comunas
         comuna = str_replace_all(comuna, "COMUNA (\\d+)", function(x) {num <- as.integer(str_extract(x, "\\d+"))
         paste0("COMUNA ", str_pad(num, width = 2, pad = "0"))}))%>% # Normalizo en 2 dígitos la numeración de comunas
  rename(name=comuna) %>% # Renombro la columna para que machee con la grilla de CABA
  merge(caba[c("name", "code")], by = "name", all.x = TRUE) %>% # Le agrego la columna de códigos de comuna para hacer la grilla
  mutate(etapa = factor(etapa, levels = c("2° Gestión HRL",
                                          "1° Gestión HRL",
                                          "Gestión MM"))) # Ordeno como las etapas deben aparecer en el gráfico


# Genero el gráfico facetado georeferenciado según la ubicación espacial de la comunas empleando la 
# librería #geofacet#

ggplot(base3, aes(etapa, ptos_wifi, fill=etapa)) +
  geom_col() + 
  geom_text(aes(label = ptos_wifi), vjust = 0, hjust = -0.1, size = 3,
            fontface = "bold", color = "black") + # Agrego etiquetas a las barras
  coord_flip()+
  facet_geo(~name, grid = caba)+
  scale_fill_manual(breaks = c("Gestión MM", "1° Gestión HRL", "2° Gestión HRL"), 
                    values=c("yellow","darkgreen","#046C9A"))+ #establezco los colores por Gestión
  scale_x_discrete(breaks = NULL)+
  geom_hline(aes(yintercept = (sum(base3$ptos_wifi)/15),linetype = "Puntos Wi-Fi promedio por comuna"),
             color = "red",
             name = "Puntos Wi-Fi promedio por comuna")+ # Agrego el valor promedio por comuna
  scale_linetype_manual(values = "dashed", name = NULL) + # Defino estilo de línea
  geom_text(aes(x = max(as.numeric(as.factor(as.numeric(etapa)))), 
                y = (sum(base3$ptos_wifi)/15),
                label = paste(round(sum(base3$ptos_wifi)/15, 2))),
            vjust = 3.5, hjust = -0.5, size = 3, color = "red", fontface = "bold") + #Agrego etiqueta de la línea
  ggtitle("Distribución de Puntos Wi por comuna y gestion municipal") + 
  ylab("Cantidad de Puntos Wi-Fi implementados por gestión") + 
  xlab("")+ 
  labs(fill = "Gestiones")+ 
  theme(legend.position="top",
        axis.title.y = element_text(size = 20), 
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5))

#Conclusiones del gráfico

# Como se evidenció al principio del trabajo la gestión previa a HRL (Mauricio Macri) fue la que creo mayor
# cantidad Puntos Wi-Fi. Sin embargo, si observamos su ejecución por comuna, la 2° Gestión HRL genero más
# Puntos Wi-Fi en las comunas 10, 11, 12 y 13. 

# Asimismo se observa que durante la 1° Gestión HRL casi no se generaron Puntos Wi-Fi. En general todas
# las comunas tuvieron una ejecución dispar salvo comuna 11, aún así, dicha comuna no alcanza la cantidad
# de Puntos Wi-Fi promedió por comuna (44,8)

# A su vez, si analizamos la ejecución a nivel agregado y la comparamos contra la cantidad de Puntos Wi-Fi
# promedio por comuna, se evidencia que las comunas 2, 5, 6, 9, 10, 11, 12 y 15; tienen una cantidad de 
# de Puntos Wi-Fi inferior al promedio comunal. 



#VISUALIZACIÓN UTILIZANDO geom_sf

#Primero modifico la base de Puntos Wi-Fi para adecuarla a mis necesidades

base4 <- base %>% 
  select(etapa,comuna) %>% 
  filter(etapa !="" & comuna != "") %>% # Elimino los Puntos Wi-Fi que no se corresponden con ninguna etapa o comuna
  rename(name = comuna) %>% # Cambio el nombre de la columna
  mutate(name = gsub("Comuna", "COMUNA", name), # corrijo los nombres de las comunas
         name = str_replace_all(name, "COMUNA (\\d+)", function(x) {num <- as.integer(str_extract(x, "\\d+"))
         paste0("COMUNA ", str_pad(num, width = 2, pad = "0"))}))%>% # Normalizo en 2 dígitos la numeración de comunas
  mutate(etapa = recode(etapa,"Gestión previa HRL"="gestion_MM",
                        "Primera etapa HRL"= "gestion_HRL",
                        "Segunda etapa HRL"= "gestion_HRL",
                        "Segunda Etapa HRL" = "gestion_HRL",
                        "Ejecutados 2017"="gestion_HRL")) %>% # Corrijo nombre de categorías
  group_by(name,etapa) %>% # Agrupo los Puntos Wi-Fi por comuna y etapa
  summarize(ptos_wifi = n()) %>%  # Cuento los Puntos Wi-Fi y los pongo en una nueva columna
  pivot_wider(names_from = etapa, values_from = ptos_wifi, values_fill = 0) 

# Ahora para poder ver la distribución espacial de los Puntos Wi-Fi a lo largo de la ciudad, empleo la 
# librería "sf" e inspecciono el archivo comunas, un shapefile que contiene los datos espaciales de la CABA. 

colnames (comunas)

# El archivo possee 7 columnas (ID, OBJETO, COMUNAS, BARRIOS, PERIMETRO, AREA y geometry), de la cuales me interesa
# "geometry"" que me brinda las coordenadas de cada columna. Reduzco el archivo a las variables necesarias y le unifico
# los datos de Puntos Wi-Fi

mapa <- comunas %>% 
  select(3,6,7) %>%
  rename(name=COMUNAS) %>%
  mutate(name = str_pad(name, width = 2, pad = "0"), # Normalizo en 2 dígitos la numeración de comunas
         name = paste0("COMUNA ", name), # Corrijo los nombres de las comunas
         area_ha = AREA/10000) %>% # Paso el área a hectáreas 
  left_join(base4,by="name") %>% 
  mutate(gestion_HRL_ha = gestion_HRL / area_ha,
         gestion_MM_ha = gestion_MM / area_ha) # Genero la variable Puntos Wi-Fi/ha para cada gestión

#Ahora genero un mapa coroplético para cada gestión según la densidad de Puntos Wi-Fi por hectárea

plot_MM <- ggplot(mapa)+
  geom_sf(aes(fill = gestion_MM_ha), color = "#8a8a8a") +
  labs(subtitle = "Puntos Wi-Fi por ha en la Gestión MM") +
  scale_fill_distiller(palette = "Blues", direction = 1,
                       guide = guide_legend(direction = 'horizontal',
                                            title.position = 'top',
                                            title.hjust = .5,
                                            label.hjust = .5, keywidth = 1, keyheight = 1)) +
  geom_sf_label(aes(label = paste0(name)), size = 2.3) + 
  theme_light() +
  theme(title = element_text(face = 'bold'),
        legend.position = 'bottom')

plot_HRL <- ggplot(mapa)+
  geom_sf(aes(fill = gestion_HRL_ha), color = "darkgray") +
  labs(subtitle = "Puntos Wi-Fi por ha en la Gestión HRL") +
  scale_fill_distiller(palette = "Blues", direction = 1,
                       guide = guide_legend(direction = 'horizontal',
                                            title.position = 'top',
                                            title.hjust = .5,
                                            label.hjust = .5, keywidth = 1, keyheight = 1)) +
  geom_sf_label(aes(label = paste0(name)), size = 2.3) + 
  theme_light() +
  theme(title = element_text(face = 'bold'),
        legend.position = 'bottom')

#Con la librería "gridExtra" muestro los dos mapas en paralelo para un mejor anlisis

plot01 <- grid.arrange(plot_MM, plot_HRL, nrow = 1)
  
# CONCLUSIONES

# A partir de gráfico se puede evidenciar que la implementación de Puntos Wi-Fi:
  
# 1. Tuvo una fuerte concentración por hectárea en la Comuna 1 y 3 en ambas gestiones
# 2. Si se comparan ambas gestiones se observa claramente que la implementación de MM se hizo alrededor de la 
#    de las comunas céntricas y hacia el sur, mientras que la gestión de HRL desarrolló una estrategia de 
#    implementación más diversificada con un tendencia hacia el norte y el oeste.