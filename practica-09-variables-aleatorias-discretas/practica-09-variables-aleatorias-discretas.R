# Graficos para la lista de ejercicios - Variables aleatorias discretas
# Ejecutar desde la carpeta de la practica:  Rscript practica-09-variables-aleatorias-discretas.R
# Todos los graficos se guardan en img/ como PDF

dir.create("img", showWarnings = FALSE)

azul  <- "#064B5A"
teal  <- "#097289"
rojo  <- "#E52E2E"
verde <- "#A277FF"
gris  <- "gray45"
base  <- adjustcolor("#097289", 0.35)

abrir <- function(nombre, w = 6, h = 4) {
  pdf(file.path("img", paste0(nombre, ".pdf")), width = w, height = h)
  par(mar = c(4, 4, 1.8, 1), las = 1, cex.axis = 0.85, mgp = c(2.6, 0.7, 0))
}
# diagrama de barras de una pmf, resaltando los valores de 'marca'
barras <- function(x, px, marca = FALSE, col_marca = rojo, ylab = "P(X = x)", xlab = "x", ...) {
  plot(x, px, type = "h", lwd = 5, lend = 1, col = ifelse(marca, col_marca, base),
       xlab = xlab, ylab = ylab, panel.first = grid(col = "gray90"), ...)
  abline(h = 0, col = "gray70")
}
titulo <- function(t) title(main = t, cex.main = 0.9, font.main = 1)

# ---------------------------------------------------------------
# Ej 1: X ~ Binomial(12, 0.21)
abrir("ej01-binomial", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
x <- 0:12; px <- dbinom(x, 12, 0.21)
barras(x, px, x == 4); titulo("P(X = 4) = 0.1460")
barras(x, px, x >= 2); titulo("P(X >= 2) = 0.7524")
abline(v = 12*0.21, lty = 2, col = azul)
text(12*0.21, max(px), "E[X] = 2.52", pos = 4, cex = 0.8, col = azul)
dev.off()

# ---------------------------------------------------------------
# Ej 2: Y ~ Poisson(5)
abrir("ej02-poisson", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
y <- 0:16; py <- dpois(y, 5)
barras(y, py, y == 2, ylab = "P(Y = y)", xlab = "y"); titulo("P(Y = 2) = 0.0842")
barras(y, py, y >= 7, ylab = "P(Y = y)", xlab = "y"); titulo("P(Y >= 7) = 0.2378")
abline(v = 5, lty = 2, col = azul)
dev.off()

# ---------------------------------------------------------------
# Ej 3: T ~ Geom(0.004), T = numero de minuto del primer fallo
abrir("ej03-geometrica", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
t <- 1:60; pt <- 0.996^(t - 1) * 0.004
barras(t, pt, t == 15, ylab = "P(T = t)", xlab = "t", ylim = c(0, 0.0045)); titulo("P(T = 15) = 0.00378")
curve(0.996^x, 0, 800, lwd = 2, col = azul, xlab = "t", ylab = "P(T > t)",
      panel.first = grid(col = "gray90"))
points(50, 0.996^50, pch = 19, col = rojo)
segments(50, 0, 50, 0.996^50, lty = 2, col = rojo); segments(0, 0.996^50, 50, 0.996^50, lty = 2, col = rojo)
text(50, 0.996^50, "  P(T > 50) = 0.8184", pos = 4, cex = 0.8, col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej 4: binomial negativa, r = 4, p = 0.10
abrir("ej04-binomial-negativa")
x <- 4:120; px <- choose(x - 1, 3) * 0.1^4 * 0.9^(x - 4)
barras(x, px, x == 10); titulo("P(X = 10) = 0.00446")
abline(v = 40, lty = 2, col = azul)
text(40, max(px), "E[X] = 40", pos = 4, cex = 0.8, col = azul)
dev.off()

# ---------------------------------------------------------------
# Ej 5: X ~ Hipergeom(N = 200, K = 30, n = 12)
abrir("ej05-hipergeometrica", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
x <- 0:12; px <- dhyper(x, 30, 170, 12)
barras(x, px, x == 2); titulo("P(X = 2) = 0.3021")
barras(x, px, x >= 3); titulo("P(X >= 3) = 0.2610")
dev.off()

# ---------------------------------------------------------------
# Ej 6: distribucion discreta del engagement
abrir("ej06-engagement")
x <- 0:5; px <- c(0.05, 0.20, 0.35, 0.25, 0.10, 0.05)
barras(x, px, x <= 2 | x >= 4, col_marca = ifelse(x <= 2, rojo, verde), ylim = c(0, 0.4))
abline(v = 2.3, lty = 2, col = azul)
text(2.3, 0.38, "E[X] = 2.3", pos = 4, cex = 0.8, col = azul)
legend("topright", c("X <= 2  (0.60)", "X >= 4  (0.15)"), col = c(rojo, verde),
       lwd = 5, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej 7: X ~ Binomial(80, 0.14)
abrir("ej07-spam", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
x <- 0:30; px <- dbinom(x, 80, 0.14)
barras(x, px, x == 12); titulo("P(X = 12) = 0.1201")
barras(x, px, x > 20); titulo("P(X > 20) = 0.0028")
abline(v = 11.2, lty = 2, col = azul)
dev.off()

# ---------------------------------------------------------------
# Ej A1: X ~ Poisson(8)
abrir("ejA1-poisson", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
x <- 0:22; px <- dpois(x, 8)
barras(x, px, x == 5); titulo("P(X = 5) = 0.0916")
barras(x, px, (x >= 6 & x <= 12) | x > 15, col_marca = ifelse(x > 15, verde, rojo))
legend("topright", c("6 <= X <= 12  (0.7450)", "X > 15  (0.0082)"), col = c(rojo, verde),
       lwd = 5, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej A2: X ~ Binomial(100, 0.87)
abrir("ejA2-binomial-ml", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
x <- 70:100; px <- dbinom(x, 100, 0.87)
barras(x, px, x == 90); titulo("P(X = 90) = 0.0860")
barras(x, px, x >= 85); titulo("P(X >= 85) = 0.7764")
abline(v = 87, lty = 2, col = azul)
dev.off()

# ---------------------------------------------------------------
# Ej A3: binomial negativa, r = 5, p = 0.12
abrir("ejA3-reintentos")
t <- 5:120; pt <- choose(t - 1, 4) * 0.12^5 * 0.88^(t - 5)
barras(t, pt, t == 12, ylab = "P(T = t)", xlab = "t"); titulo("P(T = 12) = 0.00336")
abline(v = 5/0.12, lty = 2, col = azul)
text(5/0.12, max(pt), "E[T] = 41.67", pos = 4, cex = 0.8, col = azul)
dev.off()

# ---------------------------------------------------------------
# Ej A4: T ~ Geom(0.03)
abrir("ejA4-primer-clic", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
t <- 1:80; pt <- 0.97^(t - 1) * 0.03
barras(t, pt, t == 20, ylab = "P(T = t)", xlab = "t"); titulo("P(T = 20) = 0.0168")
curve(0.97^x, 0, 150, lwd = 2, col = azul, xlab = "t", ylab = "P(T > t)",
      panel.first = grid(col = "gray90"))
points(40, 0.97^40, pch = 19, col = rojo)
segments(40, 0, 40, 0.97^40, lty = 2, col = rojo)
text(40, 0.97^40, "  P(T > 40) = 0.2957", pos = 4, cex = 0.8, col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej A5: X ~ Hipergeom(10000, 720, 50) vs aproximacion binomial
abrir("ejA5-fraude")
x <- 0:12; px <- dhyper(x, 720, 9280, 50)
barras(x, px, x == 3 | x >= 5, col_marca = ifelse(x == 3, rojo, verde))
points(x, dbinom(x, 50, 0.072), pch = 4, col = azul)
legend("topright", c("X = 3  (0.2187)", "X >= 5  (0.2903)", "Binomial(50, 0.072)"),
       col = c(rojo, verde, azul), lwd = c(5, 5, NA), pch = c(NA, NA, 4),
       bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej A6: visitas Poisson(6) y compras Y ~ Poisson(0.3)
abrir("ejA6-compras", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
v <- 0:15
barras(v, dpois(v, 6), FALSE, ylab = "P(V = v)", xlab = "v"); titulo("Visitas por minuto: Poisson(6)")
y <- 0:5; py <- dpois(y, 0.3)
barras(y, py, y == 2, ylab = "P(Y = y)", xlab = "y"); titulo("Compras por minuto: Poisson(0.3),  P(Y = 2) = 0.0333")
dev.off()

# ---------------------------------------------------------------
# Ej A7: fallos en red
abrir("ejA7-fallos")
x <- 0:3; px <- c(0.40, 0.30, 0.20, 0.10)
barras(x, px, FALSE, ylim = c(0, 0.45), xlim = c(-0.5, 3.5))
abline(v = 1, lty = 2, col = azul)
arrows(0, 0.43, 2, 0.43, code = 3, length = 0.08, col = rojo)
text(1, 0.43, "E[X] +- sd = 1 +- 1", pos = 3, cex = 0.8, col = rojo, xpd = TRUE)
dev.off()

# ---------------------------------------------------------------
# Ej A8: Poisson(10) truncada en N <= 15
abrir("ejA8-truncada", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
n <- 0:22
barras(n, dpois(n, 10), n > 15, col_marca = "gray70", ylab = "P(N = n)", xlab = "n", ylim = c(0, 0.14))
titulo("Original Poisson(10): se eliminan N > 15")
nt <- 0:15; pt <- dpois(nt, 10) / ppois(15, 10)
barras(nt, pt, nt == 12, ylab = "P(N = n | N <= 15)", xlab = "n", xlim = c(0, 22), ylim = c(0, 0.14))
titulo("Truncada:  P(N = 12 | N <= 15) = 0.0996")
dev.off()

# ---------------------------------------------------------------
# Ej A9: A/B testing, XA ~ Poisson(12), XB ~ Poisson(15)
abrir("ejA9-ab-testing", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
k <- 0:32
plot(k - 0.18, dpois(k, 12), type = "h", lwd = 3, lend = 1, col = rojo, xlab = "clics",
     ylab = "P", panel.first = grid(col = "gray90"))
lines(k + 0.18, dpois(k, 15), type = "h", lwd = 3, lend = 1, col = azul)
legend("topright", c("A ~ Poisson(12)", "B ~ Poisson(15)"), col = c(rojo, azul), lwd = 3,
       bg = "white", cex = 0.8)
d <- -20:15
pd <- sapply(d, function(z) sum(dpois(0:80, 15) * dpois(0:80 + z, 12)))
barras(d, pd, d > 0, ylab = "P(D = d)", xlab = "d")
titulo("D = XA - XB:  P(D > 0) = 0.2497")
dev.off()

# ---------------------------------------------------------------
# Ej A10: X ~ Binomial(200, 0.92)
abrir("ejA10-voz")
x <- 165:200; px <- dbinom(x, 200, 0.92)
barras(x, px, x < 180 | x > 190, col_marca = ifelse(x < 180, rojo, verde))
abline(v = 184, lty = 2, col = azul)
legend("topleft", c("X < 180  (0.1225)", "X > 190  (0.0374)"), col = c(rojo, verde),
       lwd = 5, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej A11: segundos hasta el fallo de CPU, Geom(0.0008)
abrir("ejA11-cpu")
curve(0.9992^x, 0, 5000, lwd = 2, col = azul, xlab = "t (segundos)", ylab = "P(T > t)",
      panel.first = grid(col = "gray90"))
points(500, 0.9992^500, pch = 19, col = rojo)
text(500, 0.9992^500, "  P(T > 500) = 0.6702", pos = 4, cex = 0.8, col = rojo)
abline(v = 1250, lty = 2, col = verde)
text(1250, 0.95, "E[T] = 1250 s", pos = 4, cex = 0.8, col = verde)
dev.off()

# ---------------------------------------------------------------
# Ej A12: multinomial, conteos esperados vs observados
abrir("ejA12-multinomial")
m <- rbind(esperado = c(10, 6, 4), observado = c(12, 6, 2))
barplot(m, beside = TRUE, names.arg = c("A", "B", "C"), col = c(base, rojo), border = NA,
        ylim = c(0, 14), ylab = "conteo en 20 interacciones")
legend("topright", c("E[Xi] = 20 pi", "caso (12, 6, 2)"), fill = c(base, rojo),
       border = NA, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej A13: X ~ Poisson(0.8)
abrir("ejA13-raros")
x <- 0:6; px <- dpois(x, 0.8)
barras(x, px, x == 0 | x > 2, col_marca = ifelse(x == 0, rojo, verde), ylim = c(0, 0.5))
legend("topright", c("X = 0  (0.4493)", "X > 2  (0.0474)"), col = c(rojo, verde),
       lwd = 5, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej A14: P(X = k) = c / k^2
abrir("ejA14-regla")
k <- 1:5; ck <- 1 / sum(1 / k^2)
barras(k, ck / k^2, FALSE, ylim = c(0, 0.75))
text(k, ck / k^2, sprintf("%.4f", ck / k^2), pos = 3, cex = 0.75)
abline(v = 1.5601, lty = 2, col = rojo)
text(1.5601, 0.6, "E[X] = 1.5601", pos = 4, cex = 0.8, col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej A15: X ~ Binomial(60, 0.08)
abrir("ejA15-batches", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
x <- 0:16; px <- dbinom(x, 60, 0.08)
barras(x, px, x == 7); titulo("P(X = 7) = 0.0975")
barras(x, px, x >= 10); titulo("P(X >= 10) = 0.0200")
abline(v = 4.8, lty = 2, col = azul)
dev.off()
