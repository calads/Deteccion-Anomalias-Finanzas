# Pablo Calabró
# Detección de Anomalías en Transacciones Financieras con Lenguaje R y Power BI

# Verifica el directorio de trabajo
getwd()
setwd('/Users/pablo/Documents/DataScience/PowerBI/PowerBIDSA/Cap17/Blog/')

# Instala los paquetes
install.packages("tidyverse")
install.packages("dplyr")
install.packages("solitude")
install.packages("ggplot2")
install.packages("readr")

# Cargar paquetes en esta sesión de R
library(tidyverse)
library(dplyr)
library(solitude)
library(ggplot2)
library(readr)

# Cargar datos históricos
datos_historicos <- read_csv("datos_historicos.csv")
View(datos_historicos)

# Crea el modelo de Machine Learning con el algoritmo isolationForest
?isolationForest 
modelo_ml = isolationForest$new() 

# Entrena el modelo
modelo_ml$fit(datos_historicos)

# Hace las predicciones con el modelo usando datos históricos
predicciones_historico = datos_historicos %>%
  modelo_ml$predict() %>%
  arrange(desc(anomaly_score))

View(predicciones_historico)

# Density Plot 
plot(density(predicciones_historico$anomaly_score))

# Cuanto mayor sea el anomaly score, mayor será la posibilidad de que el registro sea una anomalía
# Definamos como regla que una anomaly score superior a 0,62 es una anomalía
indices_historico = predicciones_historico[which(predicciones_historico$anomaly_score > 0.62)]

# Hace el filtro
anomalias_historico = datos_historicos[indices_historico$id, ]
normales_historico = datos_historicos[-indices_historico$id, ]

# Gráfico
colors()
ggplot() + 
  geom_point(data = normales_historico, 
             mapping = aes(trans1,trans2), 
             col = "#04A777", 
             alpha = 0.5) + 
  geom_point(data = anomalias_historico,
             mapping = aes(trans1,trans2), 
             col = "#3B341F", 
             alpha = 0.8)

# Ahora cargamos los datos
nuevos_datos <- read.csv("nuevos_datos.csv")
View(nuevos_datos)

# Predicciones con el modelo entrenado
predicciones_nuevos_datos = modelo_ml$predict(nuevos_datos)

# Se o anomaly score é maior que 0.62 consideramos como anomalia
indices_nuevos_datos = predicciones_nuevos_datos[which(predicciones_nuevos_datos$anomaly_score > 0.62)]

# Filtro
anomalias_nuevos_datos = nuevos_datos[indices_nuevos_datos$id, ]
normales_novos_dados = nuevos_datos[-indices_nuevos_datos$id, ]

# Gráfico de predicciones
ggplot() + 
  geom_point(data = normales_novos_dados, 
             mapping = aes(trans1,trans2), 
             col = "#04A777", 
             alpha = 0.5) + 
  geom_point(data = anomalias_nuevos_datos, 
             mapping = aes(trans1,trans2), 
             col = "#3B341F", 
             alpha = 0.8)

View(predicciones_nuevos_datos)

# Redondeando la columna 'anomaly_score' a 2 decimales
predicciones_nuevos_datos <- predicciones_nuevos_datos %>%
  mutate(anomaly_score = round(anomaly_score, 2))

View(predicciones_nuevos_datos)

# Creando una nueva columna basada en la condición
predicciones_nuevos_datos <- predicciones_nuevos_datos %>%
  mutate(status = ifelse(anomaly_score > 0.62, "anomalia", "normal"))

View(predicciones_nuevos_datos)

library(ggplot2)

# Creando box plot
ggplot(predicciones_nuevos_datos, aes(x = status, y = anomaly_score, fill = status)) +
  geom_boxplot() +
  labs(title = "Box Plot de Anomalias y Normales",
       x = "Status",
       y = "Anomaly Score") +
  theme_minimal() +
  scale_fill_manual(values = c("anomalia" = "#3B341F", "normal" = "#04A777")) +
  theme(legend.position = "none")

# Guardo en el disco
write.csv(predicciones_nuevos_datos, "predicciones_nuevos_datos.csv")


