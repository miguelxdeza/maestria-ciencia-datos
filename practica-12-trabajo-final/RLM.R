# Estadistica en Ciencia de Datos - Trabajo final
# Ejercicio: Regresion lineal multiple y seleccion backward (AIC)
# Base de datos: student.csv (UCI)          Autor: Miguel Deza
# Ejecutar en la carpeta que contiene student.csv:  Rscript RLM.R
# Paquetes: car, lmtest. Los graficos se guardan en img/ como PDF.
# Cada bloque "# >> nombre" ... "# <<" corresponde a una parte del informe RLM.pdf.

suppressPackageStartupMessages({ library(car); library(lmtest) })
options(width = 88, digits = 4, scipen = 6)
dir.create("img", showWarnings = FALSE)

azul  <- "#064B5A"
teal  <- "#097289"
cian  <- "#0FC5ED"
rojo  <- "#E52E2E"
lila  <- "#A277FF"
gris  <- "gray45"
pal   <- c(azul, rojo, lila, "#E8A33D")

abrir <- function(nombre, w = 6, h = 4) {
  pdf(file.path("img", paste0(nombre, ".pdf")), width = w, height = h)
  par(mar = c(4, 4, 1.8, 1), las = 1, cex.axis = 0.85, mgp = c(2.6, 0.7, 0))
}
titulo <- function(t) title(main = t, cex.main = 0.9, font.main = 1)


# >> rlm-datos
est <- read.csv("student.csv")
est$internet_casa  <- factor(est$internet_casa,  levels = c("no", "yes"))
est$apoyo_familiar <- factor(est$apoyo_familiar, levels = c("no", "yes"))
vars <- c("nota_final", "edad", "tiempo_estudio", "inasistencias",
          "reprobaciones_previas", "educacion_madre", "educacion_padre")
summary(est[, vars])
table(nota_final_cero = est$nota_final == 0)
# <<

# >> rlm-ajuste
m_full <- lm(nota_final ~ edad + tiempo_estudio + inasistencias +
               reprobaciones_previas + educacion_madre + educacion_padre +
               internet_casa + apoyo_familiar, data = est)
summary(m_full)
# <<

# >> rlm-r2
s <- summary(m_full)
c(R2 = s$r.squared, R2_ajustado = s$adj.r.squared,
  n = nobs(m_full), p = length(coef(m_full)) - 1)
# <<

# >> rlm-supuestos
shapiro.test(residuals(m_full))     # normalidad
bptest(m_full)                      # homocedasticidad (Breusch-Pagan)
resettest(m_full, power = 2:3)      # linealidad / forma funcional (RESET)
# <<

# >> rlm-diag1
abrir("rlm-diag1", w = 5.5, h = 6.6)
par(mfrow = c(2, 1))
plot(m_full, which = 1, col = adjustcolor(azul, 0.6), pch = 19, cex = 0.6,
     caption = "", sub.caption = "")
titulo("Residuos vs. ajustados")
plot(m_full, which = 2, col = adjustcolor(azul, 0.6), pch = 19, cex = 0.6,
     caption = "", sub.caption = "")
titulo("Q-Q normal de residuos estandarizados")
dev.off()
# <<

# >> rlm-diag2
abrir("rlm-diag2", w = 5.5, h = 6.6)
par(mfrow = c(2, 1))
plot(m_full, which = 3, col = adjustcolor(azul, 0.6), pch = 19, cex = 0.6,
     caption = "", sub.caption = "")
titulo("Escala-localizacion")
hist(est$nota_final, breaks = 0:20, col = adjustcolor(teal, 0.55), border = "white",
     main = "", xlab = "nota_final", ylab = "Frecuencia")
titulo("Distribucion de la respuesta: 38 notas iguales a 0")
dev.off()
# <<

# >> rlm-vif
vif(m_full)
# <<

# >> rlm-backward
m_back <- step(m_full, direction = "backward", k = 2)
# <<

# >> rlm-final
summary(m_back)
c(AIC_completo = AIC(m_full), AIC_reducido = AIC(m_back))
c(R2aj_completo = summary(m_full)$adj.r.squared,
  R2aj_reducido = summary(m_back)$adj.r.squared)
anova(m_back, m_full)               # F parcial de las variables eliminadas
# <<

# >> rlm-final-sup
shapiro.test(residuals(m_back))
bptest(m_back)
resettest(m_back, power = 2:3)
vif(m_back)
# <<

# >> rlm-diag-final
abrir("rlm-diag-final", w = 5.5, h = 6.6)
par(mfrow = c(2, 1))
plot(m_back, which = 1, col = adjustcolor(teal, 0.6), pch = 19, cex = 0.6,
     caption = "", sub.caption = "")
titulo("Modelo reducido: residuos vs. ajustados")
plot(m_back, which = 2, col = adjustcolor(teal, 0.6), pch = 19, cex = 0.6,
     caption = "", sub.caption = "")
titulo("Modelo reducido: Q-Q normal")
dev.off()
# <<

# >> rlm-sensibilidad
# los 38 ceros corresponden a alumnos que no rindieron la evaluacion final
m_sin0 <- lm(formula(m_back), data = subset(est, nota_final > 0))
round(cbind(con_ceros = coef(m_back), sin_ceros = coef(m_sin0)), 3)
c(R2aj_sin_ceros = summary(m_sin0)$adj.r.squared,
  shapiro_p = shapiro.test(residuals(m_sin0))$p.value,
  bp_p = unname(bptest(m_sin0)$p.value))
# <<
