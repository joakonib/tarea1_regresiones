# Cargar librerías
library(haven) # cargar datos en .sav
library(correlation) # variedad de correlaciones

library(dplyr) # procesar datos
library(apaTables) # tablas Word formato APA 
library(sjPlot) # tablas de regresión
library(rmdfiltr)
library(bookdown)
library(tinytex)

# Importar Base de datos
## Reemplaza XXXXXX por tu rut (sin dígito de verificación) antes de ejecutar
datos <- read_sav("processing/17604010.sav")

# Análisis de correlación
vars_cor <- datos[, c("cuidarse", "riesgo", "edad")]
cors <- correlation(vars_cor)
summary(cors, digits = 3) # ver resultados correlaciones
print(cors, digits = 3) # forma alternativa de ver correlaciones entre variables

# Tabla de correlaciones en APA
## Se creará un archivo Word en el directorio principal del proyecto con la tabla
apa.cor.table(vars_cor, table.number = 1,
              filename ="guia1_correlaciones_apa.doc")

kable(cors)

# Análisis de Regresión lineal múltiple
m1 <- lm(cuidarse ~ riesgo + trabaja + sexo + edad, data = datos)
summary(m1) # ver resultados del modelo
tab_model(m1, digits = 3) # crear tabla con resultados del modelo

# Tabla de Regresión en APA
## Se creará un archivo Word en el directorio principal del proyecto con la tabla
apa.reg.table(m1, table.number = 2,
              filename = "guia1_regresion_apa.doc")