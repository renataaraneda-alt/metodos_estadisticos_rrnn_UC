
# librerias ---------------------------------------------------------------

library(tidyverse)
#install.packages("tidyverse")
library(broom)
#install.packages("lme4")
#install.packages("Matrix")
library(lmerTest)
library(lme4)
#install.packages("lmtest")
#install.packages("readxl")
library(readxl)
#install.packages("rJava")
#install.packages("xlsx")
#install.packages("r Java")
library(dplyr)
library(car)
library(lmtest)
library(nlme)
library(ggplot2)

# cargo los datos ---------------------------------------------------------
library(readxl)
#datos_arboles_inventario <- read_excel("C:/Users/renat/Downloads/Métodos Estadísticos GitHub/Bases de Datos/arbolesInventario.xlsx")

datos_arboles_inventario <- read_xlsx("arbolesInventario.xlsx") %>% drop_na()

glimpse(datos_arboles_inventario)

# 1. SELECCION DE RODALES -------------------------------------------------

rodales_condiciones <- datos_arboles_inventario %>%
  group_by(rodal) %>%
  summarise(numero_parcelas = n()) %>% 
  filter(numero_parcelas >= 5) %>% 
  select(rodal) 

set.seed(123) #semilla aleatoria

rodales_aleatorios <- sample(rodales_condiciones$rodal, 30, replace = FALSE)

# datos nuevos ------------------------------------------------------------

datos_oficiales_arboles <-
  datos_arboles_inventario %>% filter(rodal %in% rodales_aleatorios)

# pasando datos a excel ---------------------------------------------------

nombre_repetido <- "Renata"
apellido_repetido <- "Araneda"

n_filas <- nrow(datos_oficiales_arboles)
nueva_columna_nombre <- rep(nombre_repetido, n_filas)
nueva_columna_apellido <- rep(apellido_repetido, n_filas)

datos_nombre_apellido <-
  data.frame(nombre = nueva_columna_nombre, apellido = nueva_columna_apellido)

datos_para_excel <-
  cbind(datos_nombre_apellido, datos_oficiales_arboles)
#falta exportarlos a excel


#grafico los datos
datos_oficiales_arboles %>%
  ggplot(aes(x = dap,
             y = altura)) + 
  geom_point(alpha = 0.2) + 
  theme_bw()

# preparación de datos ----------------------------------------------------
datos_oficiales_arboles$ln_altura <-
  log10(datos_oficiales_arboles$altura)
calculo_inverso <- function(x)
{
  inverso <- 1 / x
  return(inverso)
}
datos_oficiales_arboles$inverso_raiz_dap <-
  calculo_inverso(sqrt(datos_oficiales_arboles$dap))

# GRAFICOS ----------------------------------------------------------------

distribucion_datos_rodal <- datos_oficiales_arboles %>%
  ggplot(aes(x = dap,
             y = altura,
             colour = rodal)) +
  geom_point(alpha = 0.5) +
  theme_bw() 

plot(distribucion_datos_rodal)

distribucion_datos_parcela <-
  ggplot(datos_oficiales_arboles, aes(x = dap, y = altura, color = parcela)) +
  geom_point() +
  theme_bw()

plot(distribucion_datos_parcela)

boxplot_rodal <- datos_oficiales_arboles %>%
  ggplot(aes(
    group = rodal,
    x = dap,
    y = altura,
    colour = rodal
  )) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Relación altura-dap según rodal")+
  theme_bw()
plot(boxplot_rodal)

boxplot_parcela <- datos_oficiales_arboles %>%
  ggplot(aes(
    group = parcela,
    x = dap,
    y = altura,
    colour = parcela
  )) +
  geom_boxplot(alpha = 0.5) +
  labs(title = "Relación altura-dap según parcela")+
  theme_bw()

plot(boxplot_parcela)

boxplot_parcela <- datos_oficiales_arboles %>%
  ggplot(aes(
    group = parcela,
    x = dap,
    y = altura,
    colour = parcela
  )) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() #no funciona


# MODELOS -----------------------------------------------------------------
#REVISAR MODELOS
# y ~ x + (1 | grupo) # intercepto aleatorio
# y ~ x + (0 + x | grupo) # pendiente aleatoria
# y ~ x + (1 + x | grupo) # intercepto y pendiente aleatoria
#b0 INTERCEPTO
#b1 PENDIENTE

# a. observaciones independientes -----------------------------------------
modelo_a <- lm(ln_altura ~ inverso_raiz_dap, data = datos_oficiales_arboles)

summary(modelo_a) 


#graficar errores
datos_oficiales_arboles %>% 
  ggplot(aes(x = inverso_raiz_dap, y = ln_altura)) +
  geom_point(alpha = 0.2) + # alpha para puntos transparentes
  geom_line(aes(x = inverso_raiz_dap,
                y = modelo_a$fitted.values)) + #grafiquemos como linea
  theme_bw() 

# b. coef. b0 aleatorio para cada rodal -----------------------------------
modelo_b <-
  lmer(ln_altura ~ inverso_raiz_dap +
         (1 | rodal), data = datos_oficiales_arboles)

summary(modelo_b)
#pendiente: pendiente menor, la var no es tan importante para explicar(?), R2-ajustado
ranef(modelo_b) #interceptos para cada grupo

# c. coef. B1 aleatorio para cada rodal -----------------------------------
modelo_c <-
  lmer(ln_altura ~ inverso_raiz_dap + (0 + inverso_raiz_dap |
                                         rodal),
       data = datos_oficiales_arboles)

summary(modelo_c)

# d. coef. B0 y B1 aleatorio para cada rodal ------------------------------
modelo_d <-
  lmer(ln_altura ~  inverso_raiz_dap + (1 + inverso_raiz_dap |
                                          rodal),
       data = datos_oficiales_arboles)

summary(modelo_d)

# e. coef. B0 aleatorio para cada parcela ---------------------------------
modelo_e <-
  lmer(ln_altura ~ inverso_raiz_dap +
         (1 | parcela), data = datos_oficiales_arboles)

summary(modelo_e)

# f. coef. B1 aleatorio para cada parcela ---------------------------------
modelo_f <-
  lmer(ln_altura ~ inverso_raiz_dap + (0 + inverso_raiz_dap |
                                         parcela),
       data = datos_oficiales_arboles)


summary(modelo_f)

# g. coef. B0 y B1 aleatorio para cada parcela ----------------------------

modelo_g <-
  lmer(ln_altura ~ inverso_raiz_dap + (1 + inverso_raiz_dap |
                                         parcela),
       data = datos_oficiales_arboles)

summary(modelo_g)


# 3. SELECCION ------------------------------------------------------------
modelos_mixtos <-
  list(modelo_a,
       modelo_b,
       modelo_c,
       modelo_d,
       modelo_e,
       modelo_f,
       modelo_g) 

comparacion <- data.frame(
         modelo = c(
           "modelo_a",
           "modelo_b",
           "modelo_c",
           "modelo_d",
           "modelo_e",
           "modelo_f",
           "modelo_g"
         )
       ) %>%
  mutate(sapply(modelos_mixtos, BIC),
         sapply(modelos_mixtos, AIC))

ranef(modelo_b)
ranef(modelo_c)
ranef(modelo_d)
ranef(modelo_e)
ranef(modelo_f)
ranef(modelo_g)


#RAZÓN DE VEROSIMILITUD

AIC(modelo_a)

#conveniencia de usar para estructuras de datos como las 
#que se muestra de arboles

#GRÁFICOS DE RESIDUOS VS VARIABLES EXPLICATIVAS
#GRAFICOS DE EFECTOS PREDICHOS
#GRAFICOS DE DISTRIBUCION DE EFECTOS ALEATORIOS

#Normalidad del error
valores_ajustados_g <- fitted(modelo_g) # para valores ajustados
residuales_g <- residuals(modelo_g)
qqnorm(residuales_g) # normalidad residuos
qqline(residuales_g) # linea para verificar normalidad

#Linealidad y homocedasticidad
plot(valores_ajustados_g, residuales_g) # r base
abline(h=0, lty=2,col="red") # r base

#Normalidad del error
valores_ajustados_a <- fitted(modelo_a) # para valores ajustados
residuales_a <- residuals(modelo_a)
qqnorm(residuales_a) # normalidad residuos
qqline(residuales_a) # linea para verificar normalidad

#Linealidad y homocedasticidad
plot(valores_ajustados_a, residuales_a) # r base
abline(h=0, lty=2,col="red") # r base

durbinWatsonTest(modelo_a)
durbinWatsonTest(modelo_g)
#ANOVA: no se puede por la variabilidad, buscar bibliografía
anova_modelos <-
  data.frame(anova(modelo_b, modelo_c, modelo_d, modelo_e, modelo_f, modelo_g))
anova(modelo_a)

#R2: tampoco se puede usar para comaparar


####supuestos 
#NORMALIDAD
shapiro.test(modelo_a)
shapiro.test(modelo_b)
shapiro.test(modelo_c)
shapiro.test(modelo_d)
shapiro.test(modelo_e)
shapiro.test(modelo_f)
shapiro.test(modelo_g)
#HOMOCEDASCTICIDAD
bptest(modelo_a)
bptest(modelo_b)
bptest(modelo_c)
bptest(modelo_d)
bptest(modelo_e)
bptest(modelo_f)
bptest(modelo_g)
bptest()
#AUTOCORRELACIÓN
durbin.watson(modelo_a)
durbin.watson(modelo_b)
durbin.watson(modelo_c)
durbin.watson(modelo_d)
durbin.watson(modelo_e)
durbin.watson(modelo_f)
durbin.watson(modelo_g)


# Obtener los residuos del modelo mixto
residuos_mixtoMP <- resid(modelo_g)

# Ajustar un modelo de regresión lineal a los residuos cuadrados
modelo_residuos <- lm(residuos_mixtoMP^2 ~ inverso_raiz_dap , data=datos_oficiales_arboles)

# Realizar la prueba de Breusch-Pagan en el modelo de regresión lineal de los residuos cuadrados
bp_test <- bptest(modelo_residuos)
print(bp_test)
shapiro.test(modelo_residuos)


# 4. PENDIENTE: BONUS ---------------------------------------------------------------

mod_mix_auto_corr <- lme(ln_altura ~ inverso_raiz_dap, 
                         random = ~ 1 + inverso_raiz_dap | rodal/parcela, 
                         correlation = corAR1(form = ~ 1 | rodal/parcela), 
                         data = datos_oficiales_arboles)

anova(mod_mix_auto_corr)
BIC(mod_mix_auto_corr)
AIC(mod_mix_auto_corr)
logLik(mod_mix_auto_corr)

#Normalidad del error
valores_ajustados_corr <- fitted(mod_mix_auto_corr) # para valores ajustados
residuales_corr <- residuals(mod_mix_auto_corr)
qqnorm(residuales_corr) # normalidad residuos
qqline(residuales_corr) # linea para verificar normalidad

#Linealidad y homocedasticidad
plot(valores_ajustados_corr, residuales_corr) # r base
abline(h=0, lty=2,col="red") # r base


deviance(mod_mix_auto_corr)

#estructura de auto-correlacion en la matriz de varianza-covarianza que 
# de cuenta de al menos uno de estos niveles de anidamiento,
#produce unmejoramiento del modelo

