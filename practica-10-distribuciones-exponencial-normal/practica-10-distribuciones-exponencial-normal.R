# Graficos para la lista de ejercicios - Distribuciones exponencial y normal
# Ejecutar desde la carpeta de la practica:  Rscript practica-10-distribuciones-exponencial-normal.R
# Todos los graficos se guardan en img/ como PDF

dir.create("img", showWarnings = FALSE)

azul  <- "#064B5A"
teal  <- "#097289"
rojo  <- "#E52E2E"
verde <- "#A277FF"
gris  <- "gray45"

abrir <- function(nombre, w = 6, h = 4) {
  pdf(file.path("img", paste0(nombre, ".pdf")), width = w, height = h)
  par(mar = c(4, 4, 1.8, 1), las = 1, cex.axis = 0.85, mgp = c(2.6, 0.7, 0))
}
# curva de densidad f en [a, b]
densidad <- function(f, a, b, xlab = "x", ylab = "f(x)", ...) {
  curve(f(x), a, b, n = 500, lwd = 2, col = azul, xlab = xlab, ylab = ylab,
        panel.first = grid(col = "gray90"), ...)
  abline(h = 0, col = "gray70")
}
# sombrea el area bajo f entre 'desde' y 'hasta'
area <- function(f, desde, hasta, col = rojo) {
  x <- seq(desde, hasta, length.out = 300)
  polygon(c(desde, x, hasta), c(0, f(x), 0), col = adjustcolor(col, 0.45), border = NA)
}
marca <- function(v, txt, col = verde, y = par("usr")[4] * 0.9) {
  abline(v = v, lty = 2, col = col)
  text(v, y, txt, pos = 4, cex = 0.8, col = col)
}
titulo <- function(t) title(main = t, cex.main = 0.9, font.main = 1)

# ---------------------------------------------------------------
# Ej E1: T ~ Exp(1/8), fallas del servidor (horas)
abrir("ejE1-servidor", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(t) dexp(t, 1/8)
densidad(f, 0, 40, xlab = "t (horas)")
area(f, 0, 5); titulo("P(T < 5) = 0.4647")
marca(8 * log(2), "mediana = 5.55 h")
densidad(f, 0, 40, xlab = "t (horas)")
area(f, 12, 40, verde); titulo("P(T > 12) = 0.2231")
dev.off()

# ---------------------------------------------------------------
# Ej E2: T ~ Exp(3), tiempo de respuesta de una API (s)
abrir("ejE2-api", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(t) dexp(t, 3)
densidad(f, 0, 2, xlab = "t (s)")
area(f, 0, 0.1); area(f, 0.5, 2, verde)
legend("topright", c("P(T < 0.1) = 0.2592", "P(T > 0.5) = 0.2231"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.8)
densidad(f, 0, 2, xlab = "t (s)", ylim = c(0, 0.4))
area(f, log(100)/3, 2); titulo("P(T > t) = 0.01  ->  t = 1.535 s")
marca(log(100)/3, "t = 1.535", col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej E3: T ~ Exp(0.25), sesiones de usuario (min)
abrir("ejE3-sesiones", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(t) dexp(t, 0.25)
densidad(f, 0, 25, xlab = "t (min)")
area(f, 10, 25); area(f, 2, 6, verde)
legend("topright", c("P(T > 10) = 0.0821", "P(2 < T < 6) = 0.3834"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.8)
curve(pexp(x, 0.25), 0, 25, lwd = 2, col = azul, xlab = "t (min)", ylab = "F(t)",
      panel.first = grid(col = "gray90"))
abline(h = 0.9, lty = 2, col = rojo)
marca(log(10)/0.25, "q0.90 = 9.21 min", col = rojo, y = 0.4)
dev.off()

# ---------------------------------------------------------------
# Ej E4: T ~ Exp(1/30), anomalias IoT (min)
abrir("ejE4-iot", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(t) dexp(t, 1/30)
densidad(f, 0, 150, xlab = "t (min)")
area(f, 120, 150); area(f, 0, 15, verde)
legend("topright", c("P(T > 120) = 0.0183", "P(T < 15) = 0.3935"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.8)
curve(exp(-x/30), 0, 150, lwd = 2, col = azul, xlab = "t (min)", ylab = "P(T > t)",
      panel.first = grid(col = "gray90"))
abline(h = 0.2, lty = 2, col = rojo)
marca(-30 * log(0.2), "t = 48.28 min", col = rojo, y = 0.6)
dev.off()

# ---------------------------------------------------------------
# Ej E5: T ~ Exp(0.4), abandono en call center (min)
abrir("ejE5-call-center")
f <- function(t) dexp(t, 0.4)
densidad(f, 0, 12, xlab = "t (min)")
area(f, 0, 2); area(f, 5, 12, verde)
marca(log(4)/0.4, "P75 = 3.47 min", col = azul, y = 0.25)
legend("topright", c("P(T < 2) = 0.5507", "P(T > 5) = 0.1353"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej E6: tiempo entre clics, Exp(0.2 por minuto)
abrir("ejE6-clics")
f <- function(t) dexp(t, 0.2)
densidad(f, 0, 25, xlab = "t (min)")
area(f, 0, 2); area(f, 10, 25, verde)
marca(log(2)/0.2, "mediana = 3.47 min", col = azul, y = 0.12)
legend("topright", c("P(T < 2) = 0.3297", "P(T > 10) = 0.1353"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej N1: E ~ N(0, 2^2) y |E| (normal plegada)
abrir("ejN1-error-ml", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(e) dnorm(e, 0, 2)
densidad(f, -7, 7, xlab = "e")
area(f, 3, 7); area(f, -1, 1, verde)
legend("topright", c("P(E > 3) = 0.0668", "P(-1 < E < 1) = 0.3829"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.75)
densidad(function(u) 2 * dnorm(u, 0, 2), 0, 7, xlab = "|e|", ylab = "densidad de |E|")
marca(2 * sqrt(2/pi), "E|E| = 1.596", col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej N2: X ~ N(5, 1.2^2), salario
abrir("ejN2-salario")
f <- function(x) dnorm(x, 5, 1.2)
densidad(f, 1, 9, xlab = "salario (miles)")
area(f, 7, 9); area(f, 4, 6, verde)
marca(5 + 1.2 * qnorm(0.95), "q0.95 = 6.97", col = azul, y = 0.3)
legend("topleft", c("P(X > 7) = 0.0478", "P(4 < X < 6) = 0.5953"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej N3: T ~ N(50, 4^2), sensor de temperatura
abrir("ejN3-sensor", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(t) dnorm(t, 50, 4)
densidad(f, 35, 65, xlab = "temperatura")
area(f, 60, 65); area(f, 35, 40)
titulo("P(T > 60) = P(T < 40) = 0.0062")
densidad(f, 35, 65, xlab = "temperatura")
area(f, 43.42, 56.58, verde)
marca(59.31, "P99 = 59.31", col = rojo, y = 0.08)
titulo("Intervalo central 90%: [43.42, 56.58]")
dev.off()

# ---------------------------------------------------------------
# Ej N4: Z ~ N(1.5, 0.7^2) y p = 1 / (1 + exp(-Z))
abrir("ejN4-logit", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(z) dnorm(z, 1.5, 0.7)
densidad(f, -1, 4, xlab = "z (logit)")
area(f, -1, 0); area(f, 2, 4, verde)
legend("topleft", c("P(Z < 0) = 0.0161", "P(Z > 2) = 0.2375"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.75)
fp <- function(p) dnorm(qlogis(p), 1.5, 0.7) / (p * (1 - p))
densidad(fp, 0.3, 0.995, xlab = "p", ylab = "densidad de p")
abline(v = c(0.8176, 0.7965), lty = 2, col = c(rojo, verde))
legend("topleft", c("aprox. 1er orden: 0.8176", "valor exacto: 0.7965"), col = c(rojo, verde),
       lty = 2, bg = "white", cex = 0.75)
dev.off()

# ---------------------------------------------------------------
# Ej N5: clusters A ~ N(20, 3^2) y B ~ N(27, 2^2)
abrir("ejN5-clustering")
fa <- function(x) dnorm(x, 20, 3); fb <- function(x) dnorm(x, 27, 2)
densidad(fa, 10, 34, ylim = c(0, 0.21))
curve(fb(x), 10, 34, n = 500, lwd = 2, col = teal, add = TRUE, lty = 2)
area(fa, 25, 34); area(fb, 10, 25, verde)
abline(v = 23, col = gris)
points(c(23, 23), c(fa(23), fb(23)), pch = 19, col = c(rojo, verde))
text(23, fa(23), "fA(23) = 0.0807  ", pos = 2, cex = 0.75)
text(23, fb(23), "fB(23) = 0.0270  ", pos = 2, cex = 0.75)
legend("topright", c("A ~ N(20, 9)", "B ~ N(27, 4)"), col = c(azul, teal), lty = c(1, 2),
       lwd = 2, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej N6: Z = (X - 45) / 8
abrir("ejN6-batchnorm", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
densidad(dnorm, -4, 4, xlab = "z", ylab = "phi(z)")
area(dnorm, 2, 4); area(dnorm, -1, 1, verde)
legend("topleft", c("P(Z > 2) = 0.0228", "P(-1 < Z < 1) = 0.6827"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.75)
f <- function(x) dnorm(x, 45, 8)
densidad(f, 13, 77, xlab = "x")
area(f, 60.68, 77)
marca(60.68, "q0.975 = 60.68", col = rojo, y = 0.04)
dev.off()

# ---------------------------------------------------------------
# Ej N7: R ~ N(0, 1.5^2), ruido LIDAR (cm)
abrir("ejN7-lidar", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(r) dnorm(r, 0, 1.5)
densidad(f, -5, 5, xlab = "r (cm)")
area(f, 2, 5); area(f, -5, -2); area(f, -1, 1, verde)
legend("topright", c("P(|R| > 2) = 0.1824", "P(-1 < R < 1) = 0.4950"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.7)
densidad(function(u) 2 * dnorm(u, 0, 1.5), 0, 5, xlab = "|r| (cm)", ylab = "densidad de |R|")
marca(1.5 * sqrt(2/pi), "E|R| = 1.197 cm", col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej N8: epsilon ~ N(0, 5^2)
abrir("ejN8-regresion", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(e) dnorm(e, 0, 5)
densidad(f, -17, 17, xlab = "error")
area(f, 7, 17); area(f, -17, -7); area(f, -3, 3, verde)
legend("topright", c("P(|e| > 7) = 0.1615", "P(-3 < e < 3) = 0.4515"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.7)
densidad(f, -17, 17, xlab = "error")
area(f, -3.372, 3.372, teal)
abline(v = c(-3.372, 3.372), lty = 2, col = rojo)
text(c(-3.372, 3.372), 0.02, c("Q1 = -3.37", "Q3 = 3.37"), pos = c(2, 4), cex = 0.8, col = rojo)
titulo("50% central entre Q1 y Q3")
dev.off()

# ---------------------------------------------------------------
# Ej N9: Y ~ N(3.8, 0.6^2) y su version truncada en [1, 5]
abrir("ejN9-recomendacion", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
f <- function(y) dnorm(y, 3.8, 0.6)
densidad(f, 1.5, 6, xlab = "calificacion")
area(f, 5, 6); area(f, 3, 4, verde)
marca(5.196, "P99 = 5.20", col = azul, y = 0.45)
legend("topleft", c("P(Y > 5) = 0.0228", "P(3 <= Y <= 4) = 0.5393"), fill = adjustcolor(c(rojo, verde), 0.45),
       border = NA, bg = "white", cex = 0.7)
ft <- function(y) ifelse(y >= 1 & y <= 5, f(y) / (pnorm(5, 3.8, 0.6) - pnorm(1, 3.8, 0.6)), 0)
densidad(f, 1.5, 6, xlab = "calificacion", ylim = c(0, 0.75), lty = 2)
curve(ft(x), 1.5, 6, n = 800, lwd = 2, col = rojo, add = TRUE)
abline(v = 5, lty = 3, col = gris)
legend("topleft", c("N(3.8, 0.36)", "truncada en [1, 5]"), col = c(azul, rojo), lty = c(2, 1),
       lwd = 2, bg = "white", cex = 0.75)
dev.off()
