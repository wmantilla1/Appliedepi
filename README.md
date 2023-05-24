# Appliedepi
Ejemplo de codigo de analisis de riesgo a la salud simple en Rmarkdown y Tidyverse con salida a word,pdf y html

Evaluación de Riesgos a la salud
 (Ejemplo de código en R para Applied Epi en Rmarkdown)
Dr. Willebaldo Mantilla Rios
2023-05-23
Introducción
En el presente estudio se llevó a cabo un seguimiento del análisis de los efectos a la salud provocados por la fábrica Tarkett, la cual se dedica a la manipulación de vinil. En esta ocasión, se examinaron los síntomas manifestados por los habitantes cercanos a la fábrica, considerando la variable de que la fábrica se encuentra en operación. El propósito del estudio fue buscar casos activos de enfermedades relacionadas con la contaminación generada durante la operación de la fábrica. Los resultados de este estudio permitirán tener una mejor comprensión sobre los efectos de la contaminación en la salud de las personas que viven cerca de fábricas y otras actividades industriales. De esta forma, se podrán tomar medidas preventivas para proteger la salud de la población.
Objetivos:
Determinar si la reapertura de la fábrica está relacionada con un aumento en los casos de síntomas respiratorios, dermatológicos y otros problemas de salud en la población cercana a la fábrica.
Análisis
Los síntomas evaluados en el estudio se agruparon en cinco categorías: dermatológicos, oculares, respiratorios, gastrointestinales y otros. A continuación, se presenta un resumen de los síntomas y sus respectivas incidencias en la población estudiada:
# Crear una Tabla
# En el siguiente ejemplo de Código se genera una tabla utilizando el paquete KableExtra y dplyr:
#Utilizamos  warning=FALSE para eliminar los avisos en el reporte.

#Se utiliza suppressMessages para eliminar o suprimir los warning en el chunk para que el reporte de Rmarkdown se presente lo mas limpio posible:

suppressMessages(library(kableExtra))
suppressMessages(library(dplyr))

#Creamos el dataframe:
sintomas_data <- data.frame(
  Variable = c("Erupción cutánea", "Eritema", "Prurito",
               "Lagrimeo", "Irritación",
               "Tos", "Congestión nasal", "Irritación de garganta", "Escurrimiento nasal", "Dolor de cabeza",
               "Náuseas", "Vómito",
               "Hormigueo en brazos", "Hormigueo en piernas", "Cansancio mayor a lo habitual", "Sensación de pesadez de piernas", "Mareo", "Pérdida de apetito", "Insomnio"),
  N = c(18, 14, 14, 26, 41, 31, 44, 44, 32, 33, 7, 4, 6, 4, 19, 6, 15, 5, 19),
  Porcentaje = c(23.7, 18.4, 18.4, 34.2, 53.9, 40.8, 57.9, 57.9, 42.1, 43.4, 9.2, 5.3, 7.9, 5.3, 25, 7.9, 19.7, 6.6, 25)
)


#Acomodamos por categorías al data frame: sintomas_data
sintomas_data$Categoria <- c(
  rep("Sintomas Dermatologicos", 3),
  rep("Sintomas Oculares", 2),
  rep("Sintomas Respiratorios", 5),
  rep("Sintomas Gastrointestinales", 2),
  rep("Otros", 7)
)

#Reorganizamos las columnas
sintomas_data <- sintomas_data[, c("Categoria", "Variable", "N", "Porcentaje")]

sintomas_data <- sintomas_data %>%
  group_by(Categoria) %>%
  mutate(Categoria = ifelse(row_number() == 1, Categoria, "")) %>%
  ungroup()


#Crear la tabla con el formato con el estilo seleccionado :striped 
sintomas_kable <- sintomas_data %>%
  kable("simple", col.names = c("Categoría", "Síntoma", "N", "%")) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

#Personalizamos con etiquetas de las categorías a la tabla
sintomas_kable <- sintomas_kable %>%
  pack_rows("Sintomas Dermatologicos", 1, 3) %>%
  pack_rows("Sintomas Oculares", 4, 5) %>%
  pack_rows("Sintomas Respiratorios", 6, 10) %>%
  pack_rows("Sintomas Gastrointestinales", 11, 12) %>%
  pack_rows("Otros", 13, 19) 
#Visualizamos la tabla:
sintomas_kable
Categoría	Síntoma	N	%
Sintomas Dermatologicos	Erupción cutánea	18	23.7
	Eritema	14	18.4
	Prurito	14	18.4
Sintomas Oculares	Lagrimeo	26	34.2
	Irritación	41	53.9
Sintomas Respiratorios	Tos	31	40.8
	Congestión nasal	44	57.9
	Irritación de garganta	44	57.9
	Escurrimiento nasal	32	42.1
	Dolor de cabeza	33	43.4
Sintomas Gastrointestinales	Náuseas	7	9.2
	Vómito	4	5.3
Otros	Hormigueo en brazos	6	7.9
	Hormigueo en piernas	4	5.3
	Cansancio mayor a lo habitual	19	25.0
	Sensación de pesadez de piernas	6	7.9
	Mareo	15	19.7
	Pérdida de apetito	5	6.6
	Insomnio	19	25.0


Analsis de síntomas mediante cluster
# analsis de sintomas

#Cargar e instalar varios paquetes y dependencias:
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("fpc","NbClust","cluster","factoextra","tidyr")
ipak(packages)

library(readxl)
library(dplyr)
library(ggstatsplot)
library(ggplot2)
library(stats)
library(NbClust)

#datos:
df <- data.frame(
  Sintomas = c(
    "Erupcion cutanea",
    "Eritema",
    "Prurito",
    "Lagrimeo",
    "Irritacion",
    "Tos",
    "Congestion nasal",
    "Irritacion de garganta",
    "Escurrimiento nasal",
    "Dolor de cabeza",
    "Nauseas",
    "Vomito",
    "Hormigueo en brazos",
    "Hormigueo en piernas",
    "Cansancio mayor a lo habitual",
    "Sensación de pesadez de piernas",
    "Mareo",
    "Perdida de apetito",
    "Insomnio"
  ),
  Casos = c(
    18, 14, 14, 26, 41, 31, 44, 44, 32, 33, 7, 4, 6, 4, 19, 6, 15, 5, 19
  )
)



# paso 1 estandarizar base de datos
df_standardized <- scale(df[, -1])

# paso 2  
Asignar los nombres de las variables a la base de datos estandarizada De esta manera, la base de datos estandarizada df_standardized se puede utilizar en el análisis de clustering, y cuando se quiera hacer referencia a las variables originales, se pueden utilizar los nombres de columna que se han asignado en lugar de simplemente referirse a las columnas por su posición numérica.
colnames(df_standardized) <- colnames(df)[-1]

# paso 3  calcular la matriz de distancia

dist_matrix <- dist(df_standardized)

# imprimir la matriz de distancia
print(dist_matrix)
            1          2          3          4          5          6          7
 2  0.28835036                                                                  
 3  0.28835036 0.00000000                                                       
 4  0.57670072 0.86505108 0.86505108                                            
5  1.65801458 1.94636494 1.94636494 1.08131385                                 
 6  0.93713867 1.22548904 1.22548904 0.36043795 0.72087590                      
 7  1.87427735 2.16262771 2.16262771 1.29757663 0.21626277 0.93713867           
 8  1.87427735 2.16262771 2.16262771 1.29757663 0.21626277 0.93713867 0.00000000
 9  1.00922626 1.29757663 1.29757663 0.43252554 0.64878831 0.07208759 0.86505108
 10 1.08131385 1.36966422 1.36966422 0.50461313 0.57670072 0.14417518 0.79296349
 11 0.79296349 0.50461313 0.50461313 1.36966422 2.45097807 1.73010217 2.66724084
 12 1.00922626 0.72087590 0.72087590 1.58592699 2.66724084 1.94636494 2.88350361
 13 0.86505108 0.57670072 0.57670072 1.44175181 2.52306566 1.80218976 2.73932843
 14 1.00922626 0.72087590 0.72087590 1.58592699 2.66724084 1.94636494 2.88350361
 15 0.07208759 0.36043795 0.36043795 0.50461313 1.58592699 0.86505108 1.80218976
 16 0.86505108 0.57670072 0.57670072 1.44175181 2.52306566 1.80218976 2.73932843
 17 0.21626277 0.07208759 0.07208759 0.79296349 1.87427735 1.15340145 2.09054012
 18 0.93713867 0.64878831 0.64878831 1.51383940 2.59515325 1.87427735 2.81141602
 19 0.07208759 0.36043795 0.36043795 0.50461313 1.58592699 0.86505108 1.80218976
            8          9         10         11         12         13         14
 2                                                                              
 3                                                                              
 4                                                                              
5                                                                              
6                                                                              
 7                                                                              
 8                                                                              
 9  0.86505108                                                                  
 10 0.79296349 0.07208759                                                       
 11 2.66724084 1.80218976 1.87427735                                            
 12 2.88350361 2.01845253 2.09054012 0.21626277                                 
 13 2.73932843 1.87427735 1.94636494 0.07208759 0.14417518                      
 14 2.88350361 2.01845253 2.09054012 0.21626277 0.00000000 0.14417518           
 15 1.80218976 0.93713867 1.00922626 0.86505108 1.08131385 0.93713867 1.08131385
 16 2.73932843 1.87427735 1.94636494 0.07208759 0.14417518 0.00000000 0.14417518
 17 2.09054012 1.22548904 1.29757663 0.57670072 0.79296349 0.64878831 0.79296349
 18 2.81141602 1.94636494 2.01845253 0.14417518 0.07208759 0.07208759 0.07208759
 19 1.80218976 0.93713867 1.00922626 0.86505108 1.08131385 0.93713867 1.08131385
            15         16         17         18
 2                                             
 3                                             
 4                                             
 5                                             
6                                             
 7                                             
 8                                             
 9                                             
 10                                            
 11                                            
 12                                            
13                                            
 14                                            
 15                                            
 16 0.93713867                                 
17 0.28835036 0.64878831                      
 18 1.00922626 0.07208759 0.72087590           
 19 0.00000000 0.93713867 0.28835036 1.00922626
# paso 4 estimar el número de clústers
#Método 1 o del codo: Este método implica trazar la suma de las distancias cuadradas dentro del cluster (SSWC) en función del número de clusters. El número de clusters óptimo es el punto en el que la curva comienza a aplanarse o formar un "codo"

#calcular la suma de las distancias cuadradas dentro del cluster (SSWC)
wss <- (nrow(df_standardized) - 1) * sum(apply(df_standardized, 2, var))

#calcular la curva del método del codo
ssd <- c()
for (i in 1:10) {
  fit <- kmeans(df_standardized, i)
  ssd <- c(ssd, fit$tot.withinss)
}

#graficar la curva del método del codo
plot(1:10, ssd, type="b", xlab="Número de clusters", ylab="Suma de las distancias cuadradas dentro del cluster")
 
# metodo 2 
Método de la silueta: Este método implica calcular la silueta de cada punto en la base de datos estandarizada para diferentes valores de k (número de clusters) y seleccionar el valor de k que produce el valor más alto de silueta promedio. La silueta es una medida de cuán bien se ajusta cada punto a su propio cluster en comparación con los otros clusters. 


fviz_nbclust(df_standardized, kmeans, method = "silhouette")
 
#otro
#Cargar la librería "ggplot2"
library(ggplot2)

#Calcular el valor de "wss" (within-cluster sum of squares) para diferentes números de clusters
wss <- sapply(1:10, function(k) {
  kmeans(df_standardized, k, nstart = 10)$tot.withinss
})

#Graficar el valor de "wss" para diferentes números de clusters
ggplot(data.frame(x = 1:10, y = wss), aes(x, y)) +
  geom_point() +
  geom_line() +
  xlab("Número de clusters (k)") +
  ylab("Within-cluster sum of squares") +
  ggtitle("Método del codo (Elbow Method)")
 
# PASO 5 calculamos los  clústers OPTIMOS obtenidos anteriormente
k2 <- kmeans(df_standardized, centers = 3, nstart = 25)
k2
##K-means clustering with 3 clusters of sizes 7, 6, 6
  Cluster means:
       Casos
 1 -0.1620616
 2  1.2539447
 3 -1.0648728

Clustering vector:
 [1] 1 1 1 1 2 2 2 2 2 2 3 3 3 3 1 3 1 3 1

 Within cluster sum of squares by cluster:
 [1] 0.55529604 0.98475962 0.03810855
  (between_SS / total_SS =  91.2 %)
 
 Available components:
 
##[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
##[6] "betweenss"    "size"         "iter"         "ifault"
str(k2)
##List of 9
##$ cluster     : int [1:19] 1 1 1 1 2 2 2 2 2 2 ...
##$ centers     : num [1:3, 1] -0.162 1.254 -1.065
##..- attr(*, "dimnames")=List of 2
##.. ..$ : chr [1:3] "1" "2" "3"
##.. ..$ : chr "Casos"
##$ totss       : num 18
##$ withinss    : num [1:3] 0.5553 0.9848 0.0381
##$ tot.withinss: num 1.58
##$ betweenss   : num 16.4
##$ size        : int [1:3] 7 6 6
##iter        : int 2
##$ ifault      : int 0
##- attr(*, "class")= chr "kmeans"
# PASO 6 plotear los cluster
library(factoextra)
library(ggplot2)

kmeans_res <- kmeans(df_standardized, centers = 3, nstart = 25)

fviz_cluster(kmeans_res, data = df, stand = FALSE,
             geom = "point", ellipse.type = "convex") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle("Patrones y relaciones entre los síntomas (cluster)") +
  ylab("casos")
 
# paso 7 construir un dendograma con los closter calculados
res2 <- hcut(df_standardized, k = 3, stand = TRUE)

fviz_dend(res2, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","#41D107","#E7B800","#B533FF"))
 
# paso 8 pasar los cluster a mi df inicial para trabajar con ellos

df %>%
  mutate(Cluster = kmeans_res$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
##Cluster Sintomas Casos
##<int>    <dbl> <dbl>
##1       1       NA 17.9 
##2       2       NA 37.5 
##3       3       NA  5.33


Gráfico de barras simple
suppressMessages(library(ggplot2))
#ERUPCIÓN CUTANEA
casos_antes <- 26
casos_despues <- 18
poblacion_antes <- 68
poblacion_despues <- 76

#Calcular la incidencia antes y después de la reapertura
incidencia_antes <- (casos_antes / poblacion_antes) * 100
incidencia_despues <- (casos_despues / poblacion_despues) * 100

#Crear un marco de datos
incidencia_df <- data.frame(
  Periodo = c("Antes", "Después"),
  Incidencia = c(incidencia_antes, incidencia_despues)
)

ggplot(data = incidencia_df, aes(x = Periodo, y = Incidencia)) +
  geom_bar(stat = "identity", fill = c("#E69F00", "#56B4E9"), width = 0.7) +
  theme_minimal() +
  labs(
    title = "Erupción cutanea",
    subtitle = "Antes y después de la reapertura de la fábrica",
    x = "Periodo",
    y = "Prevalencia (%)"
  )+labs(caption = "X-squared = 2.9282, df = 1, p-value = 0.08704:No significativo; odds ratio  0.5037747  ")
 
#  Analisis entre grupos
                 
El siguiente analsis es una comparacion de grupos (no parametrico) utilizando paquetes como tidyverse y con una base externa:
# analisis de kruskal wallis
#cargamos los paquetes:
library(rstatix)

library(PupillometryR)

library(gtsummary)
library(here)
library(tidyverse)
library(readxl)
library(csvread)
# Ingresar base de datos
#Cargamos la base de datos almacenadas en un documento externo en excel y le asignamos un nombre o objeto en este caso lo llamaremos Anovapruebas, utilizando readxl o read_csv:
Anovapruebas <- read_csv("fil.csv")

#se analizan los tipos de datos que contiene la base:
glimpse(Anovapruebas)
##Rows: 67
##Columns: 3
##$ Ubicacion    <chr> "40mts", "40mts", "40mts", "40mts", "40mts", "40mts", "40…
##$ distancia    <dbl> 1.958784, 3.195052, 5.829294, 7.818374, 7.835193, 8.25444…
##$ Afectaciones <dbl> 3, 2, 2, 4, 3, 2, 5, 5, 2, 2, 2, 3, 3, 5, 4, 4, 4, 3, 3, …
#Convertimos la variable ubicación a factor utilizando mutate:

Anovapruebas <- Anovapruebas %>% 
  mutate( Ubicacion= factor(Ubicacion))



#krus <- (En caso de querer guardar la imagen creamos este objeto) #realizamos el grafico de comparacion de grupos:
ggplot(Anovapruebas, aes(x=Ubicacion, y=Afectaciones)) + 
  geom_flat_violin(aes(fill = Ubicacion), scale = "count") +
  geom_boxplot(width = 0.14, outlier.shape = NA, alpha = 0.5) +
  geom_point(position = position_jitter(width = 0.05), 
             size = 1.2, alpha = 0.6) +
  ggsci::scale_fill_jco() +
  theme_classic(base_size = 14) +
  theme(legend.position="none", 
        axis.text = element_text(size = 14))
 
#Para guardar la grafica utilizamos el siguiente codigo sin el signo de gato o numero, ponemos el nombre del archo y ajustamos las medidas:

#ggsave(krus, filename = "krusdistancias.png", width = 15, height = 9, units = "cm", dpi = 110)


# agrupamos los datos por distancias con dplyr
dis_summary <- Anovapruebas %>%
  group_by(Ubicacion) %>%
  dplyr::summarise(
    n = n(),
    na = sum(is.na(Afectaciones)),
    min = min(Afectaciones, na.rm = TRUE),
    q1 = quantile(Afectaciones, 0.25, na.rm = TRUE),
    median = quantile(Afectaciones, 0.5, na.rm = TRUE),
    q3 = quantile(Afectaciones, 0.75, na.rm = TRUE),
    max = max(Afectaciones, na.rm = TRUE),
    mean = mean(Afectaciones, na.rm = TRUE),
    sd = sd(Afectaciones, na.rm = TRUE),
    skewness = EnvStats::skewness(Afectaciones, na.rm = TRUE),
    kurtosis= EnvStats::kurtosis(Afectaciones, na.rm = TRUE)
  ) %>%
  ungroup()

dis_summary
###A tibble: 4 × 12
##Ubicacion     n    na   min    q1 median    q3   max  mean    sd skewness
##<fct>     <int> <int> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl>
##1 121mts        7     0     1   2.5      3   3       3  2.57 0.787   -1.76 
##2 40mts        30     0     2   2        3   4       5  3.17 1.09     0.340
##3 41a80mts     23     0     2   3        3   4.5     5  3.52 1.08     0.175
##4 81a120mts     7     0     1   2.5      3   3       4  2.71 0.951   -0.863
### ℹ 1 more variable: kurtosis <dbl>
# prueba de normalidad

Anovapruebas %>%
  group_by(Ubicacion) %>%
  shapiro_test(Afectaciones) %>% 
  ungroup()
### A tibble: 4 × 4
##Ubicacion variable     statistic        p
##<fct>     <chr>            <dbl>    <dbl>
##1 121mts    Afectaciones     0.646 0.000931
##2 40mts     Afectaciones     0.843 0.000449
##3 41a80mts  Afectaciones     0.856 0.00347 
##4 81a120mts Afectaciones     0.869 0.183
#como alguno de los grupos el resultado de p es menor 0.05 se considera no nomal

# prueba kruskal wallis

kruskal.test(Afectaciones ~ Ubicacion, data = Anovapruebas)
## 
##Kruskal-Wallis rank sum test
## 
##data:  Afectaciones by Ubicacion
##Kruskal-Wallis chi-squared = 4.7315, df = 3, p-value = 0.1925
