# =============================================================
# Practica 6 - Reglas de Asociacion en Lenguaje R
# Dataset: 4 transacciones, items A/B/C/E (SI/NO)
# minsup >= 0.5, minconf >= 0.6
# =============================================================

# ---- 1. Cargar las bibliotecas/paquetes necesarias ----
library(arules)
library(arulesViz)

# ---- 2. Cargar la data (CSV) ----
datos <- read.csv("dataset_reglas_asociacion.csv", stringsAsFactors = TRUE)
print(datos)

# ---- 3. Analisis Exploratorio de Datos ----
str(datos)
summary(datos)
cat("\nFrecuencia SI/NO por atributo:\n")
print(sapply(datos[, c("A", "B", "C", "E")], table))

# ---- 4. Preprocesar datos ----
# Quitamos el ID de transaccion (no es un item) y convertimos
# SI/NO a logico (TRUE = el item esta presente en la transaccion)
datos_log <- datos[, c("A", "B", "C", "E")]
datos_log <- as.data.frame(lapply(datos_log, function(x) x == "SI"))
print(datos_log)

transacciones <- as(datos_log, "transactions")
summary(transacciones)
inspect(transacciones)

png("img/frecuencia-items.png", width = 800, height = 600, res = 120)
itemFrequencyPlot(transacciones, main = "Frecuencia de items (soporte individual)",
                   col = "#097289", ylim = c(0, 1))
dev.off()

# ---- 5. Generar las reglas de asociacion ----
reglas <- apriori(transacciones,
                   parameter = list(supp = 0.5, conf = 0.6, target = "rules"),
                   control = list(verbose = FALSE))
reglas <- sort(reglas, by = "confidence")
inspect(reglas)
cat("\nNumero de reglas encontradas:", length(reglas), "\n")

# itemsets frecuentes (para completar el analisis)
itemsets <- apriori(transacciones,
                     parameter = list(supp = 0.5, target = "frequent itemsets"),
                     control = list(verbose = FALSE))
itemsets <- sort(itemsets, by = "support")
inspect(itemsets)

# ---- 6. Visualizar las reglas generadas ----
png("img/reglas-scatter.png", width = 800, height = 600, res = 120)
plot(reglas, measure = c("support", "confidence"), shading = "lift",
     main = "Reglas: soporte vs. confianza (color = lift)")
dev.off()

png("img/reglas-grafo.png", width = 900, height = 700, res = 120)
plot(reglas, method = "graph", engine = "igraph",
     main = "Grafo de reglas de asociacion")
dev.off()

png("img/reglas-agrupadas.png", width = 900, height = 700, res = 120)
plot(reglas, method = "grouped", main = "Matriz agrupada antecedente/consecuente")
dev.off()

cat("\nListo. Graficos guardados en img/\n")
