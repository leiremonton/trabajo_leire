
#- Leire Montón 
#- Trabajo individual:Nombres de hombre y de mujer


#- Descargamos la tablas de nombres de mujeres y de hombre
my_url <- "https://raw.githubusercontent.com/marcboquet/spanish-names/master/hombres.csv"
curl::curl_download(my_url, "./datos/hombres.csv")

my_url2 <- "https://raw.githubusercontent.com/marcboquet/spanish-names/master/mujeres.csv"
curl::curl_download(my_url2, "./datos/mujeres.csv")

# Cagar los datos
dfh <- rio::import("./datos/hombres.csv") #-
str(dfh)
dfm <- rio::import("./datos/mujeres.csv") #-
str(dfm)

library(tidyverse)

library(ggplot2)

library(scales)

library(plotly)

library(sf)

library(gt)

# 10 nombres de hombre más comunes

dfhrep <- dfh %>% group_by(nombre, edad_media) %>% summarise(frec = sum(frec)) %>% ungroup() %>% select(nombre, frec, edad_media) %>% slice_max(frec, n = 10)

p1 <- ggplot(dfhrep, aes(y = nombre, x = frec)) + geom_col(fill = "steelblue") + scale_x_continuous(labels = label_comma())

p1


# 10 nombres de mujer más comunes

dfmrep <- dfm %>% group_by(nombre, edad_media) %>% summarise(frec = sum(frec)) %>% ungroup() %>% select(nombre, frec, edad_media) %>% slice_max(frec, n = 10)

p2 <- ggplot(dfmrep, aes(y = nombre, x = frec)) + geom_col(fill = "steelblue") + scale_x_continuous(labels = label_comma())

p2



# De los nombre de hombre y mujer cuales tienen más edad de media

edadh <- dfh %>% group_by(nombre) %>% select(nombre, edad_media) %>% ungroup() %>% top_n(1, edad_media) 


edadm <- dfm %>% group_by(nombre) %>% select(nombre, edad_media) %>% ungroup() %>% top_n(1, edad_media) 



max_edad_media <- bind_rows(edadh, edadm)


t_1  <-  max_edad_media %>%
  gt()
t_1  <-  gt :: gt( max_edad_media )
t_1  %>%
  tab_header( title  =  " Nombres con mayor edad de media" , subtitle  = md ( " **En España** " )) %>% cols_label( nombre  =  " Nombre " ,   edad_media  =  " Edad media" ) %>% opt_stylize(style = 2, color = "pink")

#Los nombres más comunes 

comunes_totales <- bind_rows(dfhrep, dfmrep) 


p <- ggplot(comunes_totales) +  aes(y = reorder(nombre, desc(frec)), x = frec) +
  geom_point(mapping = NULL, stat = "identity", colour = "pink") +
  labs(title = "20 primeros nombres más comunes ") +
  labs(subtitle = "de hombres y mujeres en España en orden") +
  labs(caption = "50% hombres y 50% mujeres") +
  labs(x = "Frecuencia de repetición") +
  labs(y = "Nombres") +
  scale_x_continuous(labels = label_comma()) +
  theme_dark() 
p

#Nombres con edad media menor de 3 años, dejo los na de la frecuencia porque lo importante es la edad media de los individuos.

edadm_menor3 <- dfm %>% group_by(nombre)  %>% select(nombre, edad_media)   %>% ungroup() %>% filter(edad_media <= 3)

edadh_menor3 <- dfh %>% group_by(nombre) %>% select(nombre, edad_media) %>% ungroup() %>% filter(edad_media <= 3)

menores_3 <- bind_rows(edadm_menor3, edadh_menor3) 


t_2 <- menores_3 %>% gt()
t_2 <- gt::gt(menores_3)
t_2 %>% 
  tab_header(title = "Nombres más comunes entre los menores de 3 años", subtitle = md ("**Para hombres y mujeres en España**")) %>%
  cols_label( nombre  =  " Nombre "  , edad_media = "Media de edad"  )  %>%
  cols_move(columns = c(nombre), after = c(edad_media)) %>% 
  opt_stylize(style = 1, color = "red")

