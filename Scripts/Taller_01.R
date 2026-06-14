

# Librerias ---------------------------------------------------------------

library(tidyverse)
library(broom)
#install.packages("gridExtra")
library(gridExtra)
#install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(dplyr)
#install.packages("lmtest")
library(lmtest)
#install.packages("Metrics")
library(Metrics)


# cargo mis datos ---------------------------------------------------------

datos_halcones <- read.csv("halcones.csv") %>% drop_na()

datos0 <- read.csv("halcones.csv")

# PREGUNTA 1: ANÁLISIS EXPLORATORIO ---------------------------------------------------

glimpse(datos_halcones)

# Estadística Descriptiva:
tabla_estadistica001 <-
  datos_halcones %>%
  summarise(
    mean_wing = mean(Wing),
    sd_wing = sd(Wing),
    var_wing = var(Wing),
    min_wing = min(Wing),
    max_wing = max(Wing),
    median_wing = median(Wing),
    mean_weight = mean(Weight),
    sd_weight = sd(Weight),
    var_weight = var(Weight),
    min_weight = min(Weight),
    max_weight = max(Weight),
    median_weight = median(Weight),
    mean_culmen = mean(Culmen),
    sd_culmen = sd(Culmen),
    var_culmen = var(Culmen),
    min_culmen = min(Culmen),
    max_culmen = max(Culmen),
    median_culmen = median(Culmen),
    mean_hallux = mean(Hallux),
    sd_hallux = sd(Hallux),
    var_hallux = var(Hallux),
    min_hallux = min(Hallux),
    max_hallux = max(Hallux),
    median_hallux = median(Hallux),
    mean_tail = mean(Tail),
    sd_tail = sd(Tail),
    var_tail = var(Tail),
    min_tail = min(Tail),
    max_tail = max(Tail),
    median_tail = median(Tail)
  )

tabla_estadistica <-
  datos_halcones %>%
  group_by(Species) %>%
  summarise(
    mean_wing = mean(Wing),
    sd_wing = sd(Wing),
    var_wing = var(Wing),
    min_wing = min(Wing),
    max_wing = max(Wing),
    median_wing = median(Wing),
    mean_weight = mean(Weight),
    sd_weight = sd(Weight),
    var_weight = var(Weight),
    min_weight = min(Weight),
    max_weight = max(Weight),
    median_weight = median(Weight),
    mean_culmen = mean(Culmen),
    sd_culmen = sd(Culmen),
    var_culmen = var(Culmen),
    min_culmen = min(Culmen),
    max_culmen = max(Culmen),
    median_culmen = median(Culmen),
    mean_hallux = mean(Hallux),
    sd_hallux = sd(Hallux),
    var_hallux = var(Hallux),
    min_hallux = min(Hallux),
    max_hallux = max(Hallux),
    median_hallux = median(Hallux),
    mean_tail = mean(Tail),
    sd_tail = sd(Tail),
    var_tail = var(Tail),
    min_tail = min(Tail),
    max_tail = max(Tail),
    median_tail = median(Tail)
  )

# Gráficos entre variables por especie ------------------------------------

plot(datos_halcones)

WeighT_Wing_Especie <- datos_halcones %>%
  ggplot2::ggplot(aes(y = Weight, x = Wing, color = Species)) +
  ggplot2::geom_point() +
  scale_color_manual(values = c("pink", "turquoise", "yellow"))

WeighT_Culmen_Especie <- datos_halcones %>%
  ggplot2::ggplot(aes(y = Weight, x = Culmen, color = Species)) +
  ggplot2::geom_point() +
  scale_color_manual(values = c("pink", "turquoise", "yellow"))

WeighT_Hallux_Especie <- datos_halcones %>%
  ggplot2::ggplot(aes(y = Weight, x = Hallux, color = Species)) +
  ggplot2::geom_point() +
  scale_color_manual(values = c("pink", "turquoise", "yellow"))

WeighT_Tail_Especie <- datos_halcones %>%
  ggplot2::ggplot(aes(y = Weight, x = Tail, color = Species)) +
  ggplot2::geom_point() +
  scale_color_manual(values = c("pink", "turquoise", "yellow"))

grid.arrange(
  WeighT_Wing_Especie,
  WeighT_Culmen_Especie,
  WeighT_Hallux_Especie,
  WeighT_Tail_Especie,
  nrow = 2
)

conteo_Species <- count(datos_halcones, Species)
colores <- c(
  "CH" = "pink",
  "RT" = "turquoise",
  "SS" = "yellow"
)

ggplot(conteo_Species, aes_string(x = "Species", y = "n")) +
  geom_bar(stat = "identity", fill = colores) +
  labs(title = "Frecuencia de datos por especie")

# Distribución de las variables -------------------------------------------

grafico1 <- ggplot(datos_halcones, aes(x = Wing)) + geom_histogram() + ggtitle("Distribución de Wing")
grafico2 <- ggplot(datos_halcones, aes(x = Weight)) + geom_histogram() + ggtitle("Distribución de Weight")
grafico3 <- ggplot(datos_halcones, aes(x = Culmen)) + geom_histogram() + ggtitle("Distribución de Culmen")
grafico4 <- ggplot(datos_halcones, aes(x = Hallux)) + geom_histogram() + ggtitle("Distribución de Hallux")
grafico5 <- ggplot(datos_halcones, aes(x = Tail)) + geom_histogram() + ggtitle("Distribución de Tail")
grid.arrange(grafico1, grafico2, grafico3, grafico4, grafico5, nrow = 2)
    

# Matriz de correlación ---------------------------------------------------

matriz_corr <- cor(datos_halcones[, 2:6])

M_cor <- cor(datos_halcones[,2:6])
corrplot(M_cor, type = "full")
M_cor_graph <- corrplot(M_cor, method = "color", type = "full",
                        addCoef.col ="black", number.cex = 0.9)

M_cor_ellipse <- corrplot(M_cor, method = "color", type = "upper", 
                          diag = TRUE)

# PREGUNTA 2: MODELOS -------------------------------------------------------------

# Modelo 1 ----------------------------------------------------------------

modelo1 <- lm(Weight ~ Culmen + Wing + Tail, data = datos_halcones)

summary(modelo1)

plot(modelo1, which = 1)
plot(modelo1, which = 2)
plot(modelo1, which = 3)
plot(modelo1, which = 5)

tabla_modelo1  <- glance(modelo1)


qqnorm(modelo1$residuals)


# Modelo 2 ----------------------------------------------------------------

modelo2 <- lm(Weight ~ Culmen + Tail, data = datos_halcones)

summary(modelo2)

plot(modelo2, which = 1)
plot(modelo2, which = 2)
plot(modelo2, which = 3)
plot(modelo2, which = 5)

tabla_modelo2 <- glance(modelo2)

rm(tabla_modelo2)

qqnorm(modelo2$residuals)

# modelo 3 ----------------------------------------------------------------

modelo3 <- lm(Weight ~ Culmen + Wing, data = datos_halcones)

summary(modelo3)

plot(modelo3, which = 1)
plot(modelo3, which = 2)
plot(modelo3, which = 3)
plot(modelo3, which = 5)


plot(modelo3, which=4)


tabla_modelo3 <- glance(modelo3)

qqnorm(modelo3$residuals)

rmse_value_modelo1 <-
  rmse(resultados_modelos$datos_observado,
       resultados_modelos$ajuste_modelo1)
print(rmse_value_modelo1)

rmse_value_modelo2 <-
  rmse(resultados_modelos$datos_observado,
       resultados_modelos$ajuste_modelo2)
print(rmse_value_modelo2)

sqrt(mean((modelo3$fitted.values - resultados_modelos$datos_observado)^2))

rmse_value_modelo3 <-
  rmse(resultados_modelos$datos_observado,
       resultados_modelos$ajuste_modelo3)
print(rmse_value_modelo3)

# PREGUNTA 3: Verificación de supuestos -----------------------------------

# Normalidad
shapiro.test(resultados_modelos$error_modelo3)

# Heterocedasticidad
prueba_heterocedasticidad <- bptest(modelo3)
print(prueba_heterocedasticidad)

# Autocorrelación
dwtest(modelo3)

plot(residuals(modelo3),
     pch = 19,
     col = "deepskyblue1")

#Grado de multicolinealidad
car::vif(modelo3)


# ANOVA -------------------------------------------------------------------

anova(modelo1,modelo2,modelo3)

# ECM ---------------------------------------------------------------------

resultados_modelos <- tibble(
  datos_observado =
    datos_halcones$Weight,
  ajuste_modelo1 =
    modelo1$fitted.values,
  ajuste_modelo2 = modelo2$fitted.values,
  ajuste_modelo3 = modelo3$fitted.values,
  error_modelo1 = modelo1$residuals, 
  error_modelo2 = modelo2$residuals, 
  error_modelo3 = modelo3$residuals
)

resultados_modelos %>%
  summarise(ECM_modelo3 = mean(abs(error_modelo3)))


#ECM

resultados_modelos %>%
  summarise(ECM_modelo3 = mean(abs(error_modelo3)))

#MAD
mad(modelo3$residuals)

library(moments)
#install.packages("moments")
# Sesgo -------------------------------------------------------------------

skewness(modelo3$residuals)
# MAD ---------------------------------------------------------------------
mean(abs(modelo1$residuals))

