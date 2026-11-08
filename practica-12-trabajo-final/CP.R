# Estadistica en Ciencia de Datos - Trabajo final
# Ejercicio: Analisis de componentes principales
# Base de datos: student.csv (UCI)          Autor: Miguel Deza
# Ejecutar en la carpeta que contiene student.csv:  Rscript CP.R
# Paquetes: corrplot, car. Los graficos se guardan en img/ como PDF.
# Cada bloque "# >> nombre" ... "# <<" corresponde a una parte del informe CP.pdf.

suppressPackageStartupMessages({ library(corrplot); library(car) })
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


# >> acp-datos
est <- read.csv("student.csv")
vacp <- c("edad", "tiempo_estudio", "inasistencias", "reprobaciones_previas",
          "educacion_madre", "educacion_padre", "salud", "tiempo_libre",
          "salidas_sociales", "alcohol_dias_semana", "alcohol_fin_semana")
X <- as.matrix(est[, vacp])
round(cbind(media = colMeans(X), desv_est = apply(X, 2, sd),
            min = apply(X, 2, min), max = apply(X, 2, max)), 2)
# <<

# >> acp-cor
R <- cor(X)
round(R, 2)
# test de esfericidad de Bartlett: H0: R = I
n <- nrow(X); p <- ncol(X)
chi2 <- -(n - 1 - (2 * p + 5) / 6) * log(det(R))
c(chi2 = chi2, gl = p * (p - 1) / 2, p_valor = pchisq(chi2, p * (p - 1) / 2, lower.tail = FALSE))
# <<

# >> acp-corplot
png("img/acp-corplot.png", width = 6, height = 6, units = "in", res = 300)
corrplot(R, method = "color", type = "upper", addCoef.col = "black", number.cex = 0.6,
         tl.col = "black", tl.cex = 0.7, tl.srt = 45, diag = FALSE,
         col = colorRampPalette(c(rojo, "white", teal))(200))
dev.off()
# <<

# >> acp-pca
acp <- prcomp(X, center = TRUE, scale. = TRUE)   # equivale a usar la matriz R
vp <- acp$sdev^2
tab_vp <- data.frame(CP = paste0("CP", 1:p), autovalor = vp, var_pct = 100 * vp / sum(vp),
                     acumulada = 100 * cumsum(vp) / sum(vp))
print(tab_vp, digits = 3, row.names = FALSE)
c(Kaiser = sum(vp > 1), CP_80 = which(cumsum(vp) / sum(vp) >= 0.80)[1],
  CP_90 = which(cumsum(vp) / sum(vp) >= 0.90)[1])
# <<

# >> acp-scree
abrir("acp-scree", w = 5.5, h = 6.6)
par(mfrow = c(2, 1))
plot(vp, type = "b", pch = 19, col = azul, lwd = 2, xaxt = "n", xlab = "Componente",
     ylab = "Autovalor", panel.first = grid(col = "gray90"))
axis(1, at = 1:p); abline(h = 1, lty = 2, col = rojo)
titulo("Scree plot (linea roja: criterio de Kaiser)")
acum <- 100 * cumsum(vp) / sum(vp)
barplot(acum, names.arg = 1:p, col = ifelse(acum < 80, adjustcolor(teal, 0.5), azul),
        border = NA, ylim = c(0, 100), xlab = "Componente", ylab = "% acumulado")
abline(h = c(80, 90), lty = 2, col = c(rojo, lila))
titulo("Varianza explicada acumulada (80% y 90%)")
dev.off()
# <<

# >> acp-cargas
round(acp$rotation[, 1:4], 3)
round(cor(X, acp$x[, 1:2]), 3)   # correlacion variable-componente
# <<

# >> acp-cargas-fig
abrir("acp-cargas", w = 5.5, h = 6.6)
par(mfrow = c(2, 1), mar = c(4, 11, 1.8, 1))
for (j in 1:2) {
  l <- acp$rotation[, j]; o <- order(l)
  barplot(l[o], horiz = TRUE, col = ifelse(l[o] > 0, teal, rojo), border = NA,
          xlim = c(-0.65, 0.65), xlab = "Carga", cex.names = 0.75)
  abline(v = 0, col = gris)
  titulo(sprintf("Cargas de CP%d (%.1f%% de la varianza)", j, 100 * vp[j] / sum(vp)))
}
dev.off()
# <<

# >> acp-individuos
abrir("acp-individuos", w = 5.5, h = 6.6)
par(mfrow = c(2, 1))
z <- acp$x
col_alc <- adjustcolor(ifelse(est$alcohol_fin_semana >= 4, rojo, azul), 0.65)
plot(z[, 1], z[, 2], pch = 19, cex = 0.6, col = col_alc, xlab = "CP1", ylab = "CP2",
     panel.first = grid(col = "gray90"))
abline(h = 0, v = 0, col = "gray70")
legend("topright", c("alcohol fin de semana >= 4", "< 4"), pch = 19, col = c(rojo, azul),
       cex = 0.7, bty = "n")
titulo("Individuos en el plano CP1-CP2, por consumo de alcohol")
col_ap <- adjustcolor(ifelse(est$aprueba == 1, teal, lila), 0.65)
plot(z[, 1], z[, 2], pch = 19, cex = 0.6, col = col_ap, xlab = "CP1", ylab = "CP2",
     panel.first = grid(col = "gray90"))
abline(h = 0, v = 0, col = "gray70")
legend("topright", c("aprueba", "no aprueba"), pch = 19, col = c(teal, lila), cex = 0.7, bty = "n")
titulo("Mismo plano, coloreado por aprobacion")
dev.off()
# <<

# >> acp-biplot
abrir("acp-biplot", w = 6, h = 6)
biplot(acp, choices = 1:2, scale = 0, cex = c(0.4, 0.7), col = c(adjustcolor(gris, 0.6), rojo),
       xlab = "CP1", ylab = "CP2")
dev.off()
# <<

# >> acp-regresion
k <- which(cumsum(vp) / sum(vp) >= 0.80)[1]
dat_pc <- data.frame(nota_final = est$nota_final, acp$x[, 1:k])
m_pc <- lm(nota_final ~ ., data = dat_pc)
round(summary(m_pc)$coefficients, 4)
c(R2aj_CP = summary(m_pc)$adj.r.squared,
  R2aj_originales = summary(lm(est$nota_final ~ X))$adj.r.squared,
  max_VIF_CP = max(vif(m_pc)))
# <<
