#CARGAMOS LAS LIBRERIAS
library(ARDL)
library(dynlm)
library(strucchange)
library(rmarkdown)
library(forecast)
library(timeDate)
library(rmarkdown)
library(gridExtra)
library(urca)
library(tseries) # adf.test, kpss.test
library(readxl)
library(xts)

# CARGAR DATOS
BASE <- read_excel("C:/Users/Lenovo/Downloads/BASE.xlsx")
#View(BASE)

#PRUEBAS DE ESTACIONARIEDAD 
acc=ts(BASE)
acc
BASE$Periodo =as.Date(BASE$Periodo)
BASE$Periodo 
xts <- xts(BASE$Tasa_desempleo, order.by = BASE$Periodo)
plot(xts)
kpss.test(xts)
adf.test(xts)

#TRANSFORMACIÓN DE VARIABLES (LOGARITMOS)
# Fundamental en ARDL para interpretar como elasticidades
BASE$Tasa_desempleo <- log(BASE$Tasa_desempleo)
BASE$Credito_privado<- log(BASE$Credito_privado)
BASE$Credito_PYME<- log(BASE$Credito_PYME)

# Convertir a serie de tiempo (TS)
# Inicio 2010, Frecuencia 4 (Trimestral)
ts_data <- ts(BASE[, c("Tasa_desempleo", "Credito_privado","Credito_PYME")], start = c(2021, 1), frequency = 12)

# Graficar para ver las tendencias (Visualización inicial)
plot.ts(ts_data, main = "Evolución TASA DESEMPLEO - CRED. SECTOR PRIVADO - CREDITO PYME", col="blue")

# ==============================================================================
# ESTIMACIÓN DEL MODELO ARDL
# ==============================================================================


# Selección automática del mejor modelo usando criterio AIC


modelo_auto <- auto_ardl(Tasa_desempleo~ Credito_privado +Credito_PYME ,data = BASE , max_order = 4)
modelo_auto
# Ver qué modelo seleccionó 
print(modelo_auto$top_orders)
best_model <- modelo_auto$best_model
best_model
summary(best_model)
# ==============================================================================
# BOUNDS TEST (PRUEBA DE COINTEGRACIÓN)
# ==============================================================================

# H0: No existe relación de largo plazo
# Si F-Stat > I1 Bound, rechazamos H0 -> Sí hay cointegración
bounds_test <- bounds_f_test(best_model, case = 3)
bounds_test 
# ==============================================================================
#  MULTIPLICADORES (COEFICIENTES)
# ==============================================================================

# Extraer coeficientes de Corto Plazo y Largo Plazo
multiplicadores <- multipliers(best_model)
print(multiplicadores)
#==============================================================================
# PRUEBA DE ESTABILIDAD (CUSUM)
# ==============================================================================

# Convertir modelo ARDL a su forma de corrección de errores (UECM)
uecm_model <- uecm(best_model)
summary(uecm_model) # muestra el ECM

# Definimos manualmente las funciones 'd' y 'L' para que strucchange las entienda
d <- function(x) {
  c(NA, diff(x)) # Agrega un NA al inicio para mantener el largo del vector
}

L <- function(x, k = 1) {
  c(rep(NA, k), x[1:(length(x)-k)]) # Simula el rezago manualmente
}
# ----------------------------

# GRÁFICO CUSUM:
plot(efp(uecm_model$full_formula, data = uecm_model$data, type = "Rec-CUSUM"),
     main = "Prueba de Estabilidad CUSUM")

plot(residuals(best_model))

