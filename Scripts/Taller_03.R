# 1. Librerías ------------------------------------------------------------
# Se eliminaron las cargas redundantes (dplyr, ggplot2, readr ya vienen en tidyverse)
library(tidyverse) 
library(caret)
library(janitor)
library(gridExtra)
library(class)
library(pROC)
library(ROCR)

# 2. Carga de datos -------------------------------------------------------
datos_titanic <- read_csv("titanic.csv")

# 3. Limpieza e Imputación de datos ---------------------------------------

# Calculamos la moda de Embarked para usarla en la imputación
mode_embarked <- names(sort(table(datos_titanic$Embarked), decreasing = TRUE))[1]

# Pipeline secuencial: elimina columnas, convierte factores e imputa (Age y Embarked)
# Se reemplaza el ciclo for por funciones vectorizadas de dplyr (mucho más rápidas)
datos_imputados <- datos_titanic %>%
  select(-PassengerId, -Name, -Ticket, -Cabin) %>% 
  mutate(Pclass = as.factor(Pclass), 
         Survived = as.factor(Survived),
         Embarked = replace_na(Embarked, mode_embarked)) %>%
  group_by(Pclass, Sex) %>%
  mutate(Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age)) %>%
  ungroup()

# 4. Partición del dataset ------------------------------------------------
set.seed(3101)

index_train_titanic <- createDataPartition(datos_imputados$Survived,
                                           p = 0.8,
                                           list = FALSE,
                                           times = 1)

entrenamiento <- datos_imputados[index_train_titanic, ]
testeo <- datos_imputados[-index_train_titanic, ]

# 5. Configuración y Entrenamiento de Modelos (Solo en set de Entrenamiento) --
parametros_entrena <- trainControl(method = "cv", number = 4)

# Regresión Logística (RL)
modelo_log <- train(Survived ~ Sex + Pclass,
                    data = entrenamiento,
                    method = 'glm',
                    family = "binomial",
                    trControl = parametros_entrena)

# Naive Bayes (NB)
modelo_nb <- train(Survived ~ Sex + Pclass,
                   data = entrenamiento,
                   method = 'nb',
                   trControl = parametros_entrena)

# K-Nearest Neighbors (KNN) con ajuste de hiperparámetros (TuneGrid)
grid_knn <- expand.grid(k = seq(1, 20, by = 2))

modelo_knn <- train(Survived ~ Pclass + Sex,
                    data = entrenamiento,
                    preProcess = c("center", "scale"), 
                    method = 'knn',
                    tuneGrid = grid_knn,
                    trControl = parametros_entrena)

# Graficar la validación cruzada para KNN
plot_knn <- ggplot(modelo_knn, highlight = TRUE)

# 6. Evaluación de los modelos --------------------------------------------

# Comparación de modelos con Cross-Validation del Entrenamiento
resumen_modelos_cv <- resamples(list(NB = modelo_nb,
                                     LOG = modelo_log,
                                     KNN = modelo_knn))

boxplot_modelos <- bwplot(resumen_modelos_cv)

# Generación de predicciones usando los modelos frente al set de Testeo
predicciones_log <- predict(modelo_log, newdata = testeo)
predicciones_nb  <- predict(modelo_nb, newdata = testeo)
predicciones_knn <- predict(modelo_knn, newdata = testeo)

# Matrices de confusión
cm_log <- confusionMatrix(predicciones_log, testeo$Survived)
cm_nb  <- confusionMatrix(predicciones_nb, testeo$Survived)
cm_knn <- confusionMatrix(predicciones_knn, testeo$Survived)

# 7. Extracción de estadísticas y coeficientes ----------------------------

# Coeficientes significativos de la Regresión Logística (p < 0.05)
resumen_logit <- summary(modelo_log)
coeficientes <- resumen_logit$coefficients
coeficientes_significativos <- coeficientes[coeficientes[, "Pr(>|z|)"] < 0.05, , drop = FALSE]

# Probabilidades a priori usando regla de Laplace
frecuencia_surv_laplace <- (table(testeo$Survived) + 1) / (length(testeo$Survived) + length(unique(testeo$Survived)))

# Estadísticas bayesianas para Age según Pclass (Prob. a priori, media y desviación)
resultados_bayes_ingenuo <- datos_imputados %>%
  group_by(Pclass) %>%
  summarise(prob = n() / nrow(datos_imputados),
            media_age = mean(Age, na.rm = TRUE),
            desviacion_age = sd(Age, na.rm = TRUE))

