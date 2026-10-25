# Graficos para la lista de ejercicios - Estimacion por maxima verosimilitud
# Ejecutar desde la carpeta de la practica:  Rscript practica-11-maxima-verosimilitud.R
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
# curva de (log)verosimilitud con el EMV marcado
curva <- function(f, a, b, emv, xlab = expression(theta), ylab = "ln L", ...) {
  curve(f(x), a, b, n = 500, lwd = 2, col = azul, xlab = xlab, ylab = ylab,
        panel.first = grid(col = "gray90"), ...)
  abline(v = emv, lty = 2, col = rojo)
  points(emv, f(emv), pch = 19, col = rojo)
}
# densidad ajustada con los datos como marcas en el eje
ajuste <- function(f, a, b, y, xlab = "y") {
  curve(f(x), a, b, n = 500, lwd = 2, col = azul, xlab = xlab, ylab = "f(y)",
        panel.first = grid(col = "gray90"))
  rug(y, col = rojo, lwd = 2, ticksize = 0.06)
  abline(h = 0, col = "gray70")
}
titulo <- function(t) title(main = t, cex.main = 0.9, font.main = 1)

# ---------------------------------------------------------------
# Ej 5.2.1: Bernoulli, 5 unos en n = 8
abrir("ej521-bernoulli")
curva(function(t) t^5 * (1 - t)^3, 0, 1, 5/8, ylab = "L(theta)")
titulo("L = theta^5 (1 - theta)^3,  EMV = 5/8")
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.2: urna, p solo puede ser 1/3 o 1/2
abrir("ej522-urna")
L <- function(p) p^2 * (1 - p)^3
curve(L(x), 0, 1, n = 500, lwd = 2, col = "gray70", lty = 2, xlab = "p", ylab = "L(p)",
      panel.first = grid(col = "gray90"))
segments(c(1/3, 1/2), 0, c(1/3, 1/2), L(c(1/3, 1/2)), lwd = 5, lend = 1, col = c(rojo, base))
text(c(1/3, 1/2), L(c(1/3, 1/2)), c("L(1/3) = 0.0329", "L(1/2) = 0.0313"),
     pos = c(2, 4), cex = 0.8)
titulo("Solo se comparan los dos valores posibles")
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.3: exponencial
abrir("ej523-exponencial", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
y <- c(8.2, 9.1, 10.6, 4.9); lh <- 4 / sum(y)
curva(function(l) 4 * log(l) - l * sum(y), 0.02, 0.5, lh, xlab = expression(lambda))
titulo("ln L = 4 ln(lambda) - 32.8 lambda,  EMV = 0.1220")
ajuste(function(x) dexp(x, lh), 0, 30, y)
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.4: p(k; theta) = theta^(2k) e^(-theta^2) / k!  (muestra simulada)
abrir("ej524-poisson-theta2")
set.seed(1); k <- rpois(30, 4)
curva(function(t) 2 * sum(k) * log(t) - 30 * t^2, 1, 3, sqrt(mean(k)))
titulo("Muestra simulada n = 30:  EMV = sqrt(media) = 2.0166")
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.5: gamma con forma 4 y escala theta
abrir("ej525-gamma", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
y <- c(2.3, 1.9, 4.6); th <- sum(y) / 12
curva(function(t) -sum(y) / t - 12 * log(t), 0.3, 2, th)
titulo("ln L = -8.8/theta - 12 ln(theta) + cte,  EMV = 0.7333")
ajuste(function(x) x^3 * exp(-x / th) / (6 * th^4), 0, 8, y)
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.6: f(y) = theta^2 sqrt(y) exp(-theta sqrt(y))
abrir("ej526-raiz", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
y <- c(6.2, 7.0, 2.5, 4.2); th <- 8 / sum(sqrt(y))
curva(function(t) 8 * log(t) - t * sum(sqrt(y)), 0.2, 2.5, th)
titulo("ln L = 8 ln(theta) - 8.766 theta + cte,  EMV = 0.9126")
ajuste(function(x) th^2 * sqrt(x) * exp(-th * sqrt(x)), 0, 25, y)
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.7: f(y) = theta y^(theta - 1) en [0, 1]
abrir("ej527-proporciones", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
y <- c(0.77, 0.82, 0.92, 0.94, 0.98); th <- -5 / sum(log(y))
curva(function(t) 5 * log(t) + (t - 1) * sum(log(y)), 1, 20, th)
titulo("ln L = 5 ln(theta) - 0.6253 (theta - 1),  EMV = 7.996")
ajuste(function(x) th * x^(th - 1), 0, 1, y)
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.8: ocupantes por auto, geometrica con la clase 6+ censurada
abrir("ej528-ocupantes")
obs <- c(678, 227, 56, 28, 8, 14); p <- 997 / 1522
esp <- 1011 * c(p * (1 - p)^(0:4), (1 - p)^5)
barplot(rbind(obs, esp), beside = TRUE, names.arg = c(1:5, "6+"), col = c(base, rojo),
        border = NA, xlab = "ocupantes", ylab = "frecuencia", ylim = c(0, 720))
legend("topright", c("observada", "esperada (p = 0.6551)"), fill = c(base, rojo),
       border = NA, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.9: no-hitters por temporada, Poisson con la clase 4+ censurada
abrir("ej529-no-hitters", w = 5.5, h = 4.8)
par(mfrow = c(2, 1))
f <- c(6, 19, 12, 13, 9)
lnL <- function(l) sum(f[1:4] * dpois(0:3, l, log = TRUE)) +
  f[5] * ppois(3, l, lower.tail = FALSE, log.p = TRUE)
lh <- optimize(function(l) -lnL(l), c(0.5, 5))$minimum
curva(Vectorize(lnL), 1, 3.5, lh, xlab = expression(lambda))
titulo("ln L con la clase 4+ censurada,  EMV = 2.085")
esp <- 59 * c(dpois(0:3, lh), 1 - ppois(3, lh))
barplot(rbind(f, esp), beside = TRUE, names.arg = c(0:3, "4+"), col = c(base, rojo),
        border = NA, xlab = "no-hitters", ylab = "temporadas", ylim = c(0, 22))
legend("topright", c("observada", "esperada"), fill = c(base, rojo), border = NA,
       bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.10: uniforme en [0, theta] y en [theta1, theta2]
abrir("ej5210-uniforme", w = 5.5, h = 6.4)
par(mfrow = c(2, 1))
y <- c(6.3, 1.8, 14.2, 7.6)
curve(ifelse(x >= 14.2, x^-4, 0), 5, 30, n = 1000, lwd = 2, col = azul,
      xlab = expression(theta), ylab = "L(theta)", panel.first = grid(col = "gray90"))
points(14.2, 14.2^-4, pch = 19, col = rojo)
rug(y, col = rojo, lwd = 2)
titulo("L = 1/theta^4 si theta >= max(y) = 14.2")
plot(y, rep(0, 4), pch = 19, col = rojo, ylim = c(-0.5, 1), xlim = c(0, 16), yaxt = "n",
     xlab = "y", ylab = "", panel.first = grid(col = "gray90"))
rect(1.8, 0, 14.2, 1 / (14.2 - 1.8) * 6, col = base, border = azul)
text(c(1.8, 14.2), -0.3, c("theta1 = 1.8", "theta2 = 14.2"), col = rojo, cex = 0.8)
titulo("Densidad ajustada en [min(y), max(y)]")
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.11: f(y) = 2y / (1 - theta^2) en [theta, 1]
abrir("ej5211-soporte")
curve(ifelse(x <= 0.21, (1 - x^2)^-6, NA), 0, 0.21, n = 500, lwd = 2, col = azul,
      xlim = c(0, 0.6), ylim = c(1, 1.35), xlab = expression(theta), ylab = "L(theta) / cte",
      panel.first = grid(col = "gray90"))
points(0.21, (1 - 0.21^2)^-6, pch = 19, col = rojo)
abline(v = 0.21, lty = 2, col = rojo)
text(0.21, 1.3, "theta <= min(y) = 0.21", pos = 4, cex = 0.8, col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.12: f(y) = 2y / theta^2 en [0, theta]  (muestra simulada)
abrir("ej5212-triangular")
set.seed(2); y <- 3 * sqrt(runif(10))
curve(ifelse(x >= max(y), x^-20, NA), 2, 5, n = 800, lwd = 2, col = azul,
      xlab = expression(theta), ylab = "L(theta) / cte", panel.first = grid(col = "gray90"))
points(max(y), max(y)^-20, pch = 19, col = rojo)
rug(y, col = rojo, lwd = 2)
titulo(sprintf("Muestra simulada n = 10:  EMV = max(y) = %.3f", max(y)))
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.13: Pareto con k conocido (muestra simulada, k = 1, theta = 2.5)
abrir("ej5213-pareto")
set.seed(3); y <- runif(25)^(-1 / 2.5); S <- sum(log(y))
curva(function(t) 25 * log(t) - (t + 1) * S, 1, 6, 25 / S)
titulo(sprintf("Muestra simulada n = 25:  EMV = 25 / sum ln(y/k) = %.3f", 25 / S))
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.14: binomial negativa con r conocido (muestra simulada, r = 3, p = 0.4)
abrir("ej5214-binomial-negativa")
set.seed(4); k <- rnbinom(40, size = 3, prob = 0.4)
curva(function(p) 120 * log(p) + sum(k) * log(1 - p), 0.15, 0.8, 120 / (120 + sum(k)),
      xlab = "p")
titulo(sprintf("Muestra simulada n = 40:  EMV = r / (r + media) = %.3f", 120 / (120 + sum(k))))
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.15: Weibull con beta conocido (muestra simulada, beta = 2, alpha = 0.5)
abrir("ej5215-weibull")
set.seed(5); y <- rweibull(50, shape = 2, scale = 0.5^(-1/2)); S <- sum(y^2)
curva(function(a) 50 * log(a) - a * S, 0.1, 1.5, 50 / S, xlab = expression(alpha))
titulo(sprintf("Muestra simulada n = 50:  EMV = n / sum y^beta = %.3f", 50 / S))
dev.off()

# ---------------------------------------------------------------
# Ej 5.2.16: varianza con mu conocida vs mu estimada (5000 muestras, n = 10, sigma^2 = 4)
abrir("ej5216-varianza")
set.seed(6)
m <- replicate(5000, { y <- rnorm(10, 0, 2); c(mean(y^2), mean((y - mean(y))^2)) })
plot(density(m[1, ]), lwd = 2, col = azul, main = "", xlab = "estimacion de sigma^2", ylab = "densidad",
     ylim = c(0, 0.25), panel.first = grid(col = "gray90"))
lines(density(m[2, ]), lwd = 2, col = rojo, lty = 2)
abline(v = rowMeans(m), col = c(azul, rojo), lty = 3)
abline(v = 4, col = gris)
legend("topright", c(sprintf("mu conocida: media %.2f", mean(m[1, ])),
                     sprintf("mu estimada: media %.2f", mean(m[2, ])), "sigma^2 = 4"),
       col = c(azul, rojo, gris), lty = c(1, 2, 1), lwd = 2, bg = "white", cex = 0.8)
dev.off()
