#Install Packages

install.packages("cluster")
install.packages("devtools")
devtools::install_github("kassambara/factoextra")
install.packages("sampler")

#Active packages

library(cluster)
library(kassambara/factoextra)
library(tidyverse)
library("factoextra")
library(sampler)

rm(list = ls()) # Clean the environment

?kmeans

#Leer el archivo

muestra_silberman_csv <- read.csv("https://raw.githubusercontent.com/diegoteca/public_files/master/muestra_silberman.csv")
ms <- muestra_silberman_csv

#Lo convierto a tibble antes de realizarles modificaciones

as.tibble(ms)

#Le duplico algunas columnas para luego standarizarlas

ms <- ms %>% #Le pido que al conjunto de datos "ms" le saque los NA (Not Available) de las siguientes variables
        drop_na(tagua,
                tcloaca,
                thaci,
                teduc,
                tcober,
                the) %>% #A lo anterior le encadeno la función de duplicar las tasas de los radios que serán las variables que luego voy a usar como insumo en el cluster 
       mutate(tagua_st = tagua,
         tcloaca_st = tcloaca,
         thaci_st = thaci,
         teduc_st = teduc,
         tcober_st = tcober,
         the_st = the ) %>% # A lo anterior le encadeno que a las nuevas variables las estandarice, esto es, que previo calculo de la media, luego calcule los desvíos, luego calcule la suma de esos desvio al cuadrado, luego la varianza para finalmente calcular el desvio estandar. 
        mutate_at(vars(tagua_st,
                       tcloaca_st,
                       thaci_st,
                       teduc_st,
                       tcober_st,
                       the_st), .funs = list(~scale))

# Creo un archivo especifico con sólo las variables que interesan para el cluster. 
# No pongo los códigos. Después se pegan por orden

ms_cluster <- select(ms, 
                     tagua_st,
                     tcloaca_st,
                     thaci_st,
                     teduc_st,
                     tcober_st,
                     the_st)

#Ahora tengo dos conjuntos de datos
# El original modificado (ms) y
# Un subconjunto de este (ms_cluster) con solo las variables que serán servirán de insumo en el cluster

# Comienzo el análisis de cluster con el archivo ms_cluster

# De modo exploratorio calculo las distancias entre los radios teniendo como insumo las variables standarizadas

distancias<- get_dist(ms_cluster, method = "pearson")

# Teniendo como insumo el producto anterior, le pido un gráfico en forma de matriz en donde rojo es similar y azul muy distinto

similitud <- fviz_dist(distancias)
plot(similitud)
ggsave("similitud.png") #Lo guardo


# Le pido un kmeans de 8 clusters

kmean_8 <-kmeans(ms_cluster, 8, iter.max = 20, nstart = 50) # Para hacerlo más robusto le pido que inicie el algoritmo (nstart) por 50 lados diferentes

plot(ms_cluster, col = kmean_8$cluster) # Gráficos xy en funcion del valor de cada caso para cada variable
points(kmean_8$centers, col = 1:8, pch = 8, cex = 2)

# Gráfico para ver la reducción del error en función del número de clusters

kmean_8_e <-fviz_nbclust(ms_cluster, kmeans, method = "wss", nstart=50) +
  geom_vline(xintercept = 8, linetype = 2)
plot(kmean_8_e)
ggsave("kmean_8_e.png")

# Le pido que me imprima las salidas del kmean

print(kmean_8)

# kmeans_8 es un conjunto de (9) listas. Es un objeto tipo kmeans.
# Es un objeto clase 3 del cual se puede hacer print y fitted.
# Este dato es importante porque me da una idea que se puede hacer como el mismo y su respectivo modo de ejecución

class(kmean_8)

#contiene datos generales sobre el análisis que no (todos) pueden pasarse a cada radio. 
# El unico dato que se pasa, porque tiene la misma cantidad de filas que el archivo insumo, es el número de cluster de cada radio.

#Lo siguiente se sacó del paquete de la ayuda del paquete "cluster"

# sum of squares (ss). Es sólo una funcion para ser usada despues.
ss <- function(ms_cluster) sum(scale(ms_cluster, scale = TRUE)^2)

## cluster centers (kmean_8) "fitted" to each obs.(ms_cluster):

fitted.ms_cluster <- fitted(kmean_8)

# Veo algunos datos del producto anterior.

head(fitted.ms_cluster)

# Data frame con los residuos específicos de cada caso con respecto a la media de su cluster

resid.ms_cluster <- ms_cluster - fitted.ms_cluster

#Gráfico

matrix_dispersion_msclusteplot(ms_cluster, col = kmean_8$cluster)

# Acciones para agregar diferentes valores antes construidos al archivo que contiene los casos (radios)
# La lista de los números de cluster la convierto a tibble y en el mismo paso le cambio el nombre
# Las listas no tienen nombre de columna pero el tibble por defecto le pone "value". Yo cambio "value" a "cluster"


n.cluster <- as.tibble(kmean_8$cluster) %>%
  rename(num_cluster = value)

# Uno los 2 dataframes

ms <- bind_cols(ms, n.cluster)

# Convierto a tibble y renombro las variables del conjunto de datos que contenía los residuos de cada caso para cada variable

resid.ms_cluster <- as.tibble(resid.ms_cluster) %>%
  rename(r_agua = tagua_st,
         r_cloaca = tcloaca_st,
         r_haci = thaci_st,
         r_educ = teduc_st,
         r_cober = tcober_st,
         r_he = the_st)

ms <- bind_cols(ms, resid.ms_cluster)

# Creo data.frames de las listas de kmean que luego las puedo usar

kmean_betweens <- as.tibble(kmean_8$betweenss) %>%
                  rename(betweens = value)

kmean_tot.withinss <- as.tibble(kmean_8$tot.withinss) %>%
                  rename(tot.withinss = value)

kmean_totss <- as.tibble(kmean_8$totss) %>%
                 rename(totss = value)

kmean_size <- as.tibble (kmean_8$size) %>%
                rename (size_cluster = value)

kmean_withinss <- as.tibble (kmean_8$withinss) %>%
                rename (withinss = value)

                    
equality.kmean <- bind_cols(kmean_betweens, kmean_tot.withinss, kmean_totss)


# Se comienza el proceso del muestreo sobre la población "estratificada" a través de los clusters
# Se hace con el paquete "sampler". Un buen paquete, con varias versiones y muchas bajadas, es "survey".
# Creo un data.frame que tiene como columnas:
# Cantidad de observaciones (radios) por cluster
# Error total de cada clusters
# Cantidad de población agregada de cada cluster
# Numero identificatorio del cluster para pegar con el data.frame de las observaciones

datos_clusters <- bind_cols(kmean_size, kmean_withinss) 
numero_cluster <- c(1,2,3,4,5,6,7,8)
numero_cluster <- as.tibble(numero_cluster) %>%
                  rename (num_cluster = value)

datos_clusters <- bind_cols(datos_clusters, numero_cluster)

# Al ms le agrego una columna (mutate) que sume la cantidad de población por cluster

ms <- ms %>%
      group_by(num_cluster) %>%
      mutate(pob_total_cluster = sum(pobtotal))

# Me quedo con un subconjunto (select) 

pob_total_clusters <- dplyr::select(ms, num_cluster, pob_total_cluster) %>%
                      group_by(num_cluster) %>%
                      summarise( pob_total_cluster = mean(pob_total_cluster))

# Le agrego columnas (bind_cols) a datos_clusters desde pob_total_clusters)

datos_clusters <- bind_cols(datos_clusters, pob_total_clusters)

# Debo borrar los objetos kmean_size y pob_total_cluster porque se suporponen en la siguiente expresion

remove(kmean_size)
remove(pob_total_clusters)

# Con esta función le pido que calcule la cantidad de casos que debería tener cada estrato, dado el n de la muestra y la cantidad de personas por estrato
# para tener el mismo margen de error. Se asume un 30% de norespuesta. El "problema" es que cada estrato tiene una varianza diferente y eso aca no se tiene en cuenta. 

muestra_clusters <- psampcalc(df=datos_clusters, n=600, strata = num_cluster, unit = pob_total_cluster, over = 0.3)

install.packages("writexl")

library(writexl)

write_xlsx(x = muestra_clusters, path = "muestra_clusters.xlsx", col_names = TRUE)
write_xlsx(x = ms, path = "ms.xlsx", col_names = TRUE)
write_xlsx(x = datos_clusters, path = "datos_clusters.xlsx", col_names = TRUE)

# Calculo la varianza interna de cada cluster

no_respuesta <- 0.3
n_sample <- 600
n_sample_final <- 600 + (n_sample * no_respuesta)

datos_clusters <- dplyr::mutate(datos_clusters, varianza_cluster = withinss / (size_cluster-1))
datos_clusters <- dplyr::mutate(datos_clusters, st_desviation_cluster = sqrt(varianza_cluster))
datos_clusters <- dplyr::mutate(datos_clusters, porcentaje_pob = (pob_total_cluster * 100) / (sum(pob_total_cluster)))
datos_clusters <- dplyr::mutate(datos_clusters, probabilidad_pob = pob_total_cluster/ sum(pob_total_cluster))
datos_clusters <- dplyr::mutate(datos_clusters, n_casos_cluster = round(n_sample_final * probabilidad_pob))

# Se debería crear una función que, dado la varianza interna de cada cluster, logre converger el error de cada cluster ajustando la cantidad de casos a seleccionar

# Si se realiza la anterior, luego hay que traducir esos casos a puntos muestra. Esa transformación depende de la cantidad de casos por pm

# Si se elige una cantidad de casos diferente por cluster que no respete las proporiciones originales luego hay que construir ponderadores que reparen este cambio

# Con los datos de arriba ahora se puede introducir el desvío standard de cada cluster como insumo en la fórmula.
# Esto permite que se puede, despejando, calcular la cantidad de casos para que todos los clusters tengan el mismo error
# Usando como insumo la cantidad total de casos (más un margen de no respuesta), el desvío standard de cada cluster
# Eso lo hago con excel abajo sobre el archivo datos_clusters

# Luego se está en condiciones de realizar un sorteo adentro de cada clusters que cumpla la condición de un N particular para cada cluster
# Seguramente deberá haber mejores maneras de hacerlo. Por lo pronto ahora calcule de forma artesanal las cantidades en escel
# Con las líneas de abajo creo diferentes objetos, luego se "seleccionan" mediante un sample y luego pego todas las filas
# Es con replace = FALSE porque sino puede volver a salir el mismo radio 2 veces

ms_pm1 <- dplyr::filter(ms, num_cluster == 1)
ms_pm1 <- dplyr::sample_n(ms_pm1, 9, replace = FALSE)
ms_pm2 <- dplyr::filter(ms, num_cluster == 2)
ms_pm2 <- dplyr::sample_n(ms_pm2, 16, replace = FALSE)
ms_pm3 <- dplyr::filter(ms, num_cluster == 3)
ms_pm3 <- dplyr::sample_n(ms_pm3, 13, replace = FALSE)
ms_pm4 <- dplyr::filter(ms, num_cluster == 4)
ms_pm4 <- dplyr::sample_n(ms_pm4, 14, replace = FALSE)
ms_pm5 <- dplyr::filter(ms, num_cluster == 5)
ms_pm5 <- dplyr::sample_n(ms_pm5, 9, replace = FALSE)
ms_pm6 <- dplyr::filter(ms, num_cluster == 6)
ms_pm6 <- dplyr::sample_n(ms_pm6, 13, replace = FALSE)
ms_pm7 <- dplyr::filter(ms, num_cluster == 7)
ms_pm7 <- dplyr::sample_n(ms_pm7, 7, replace = FALSE)
ms_pm8 <- dplyr::filter(ms, num_cluster == 8)
ms_pm8 <- dplyr::sample_n(ms_pm8, 16, replace = FALSE)

# Pego todos los casos (filas).

ms_pm <- dplyr::bind_rows(ms_pm1, 
                          ms_pm2, 
                          ms_pm3, 
                          ms_pm4, 
                          ms_pm5,
                          ms_pm6,
                          ms_pm7,
                          ms_pm8)

# Le agrego una variable (pm) que le asigno valor 1

ms_pm <- mutate(ms_pm, pm = 1)
ms_pm <- ungroup(ms_pm)
ms_pm <- select(ms_pm, codigo, pm)

# Hago un joint com ms para pasarle la variable "pm"

ms_final <- dplyr::left_join(ms, ms_pm, by = "codigo")
ms_final <- select(ms_final, -num_cluster.y, -num_cluster.x)

# Se agrega una columna con la sumaacumulada para que ese resultado se el número de punto muestra

ms_final <- mutate (ms_final, n_pm = cumsum(pm))


write_xlsx(x = ms_final, path = "ms_final.xlsx", col_names = TRUE)