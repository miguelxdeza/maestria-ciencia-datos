# Graficos para la lista de ejercicios - Unidad I
# Ejecutar desde la carpeta de la practica:  Rscript graficos.R
# Todos los graficos se guardan en img/ como PDF

dir.create("img", showWarnings = FALSE)

azul  <- "#064B5A"
rojo  <- "#E52E2E"
verde <- "#A277FF"
gris  <- "gray45"

abrir <- function(nombre, w = 6, h = 4) {
  pdf(file.path("img", paste0(nombre, ".pdf")), width = w, height = h)
  par(mar = c(4, 4, 1.5, 1), las = 1, cex.axis = 0.85, mgp = c(2.4, 0.7, 0))
}
ejes <- function() abline(h = 0, v = 0, col = "gray70")

# ---------------------------------------------------------------
# Ej 1: f(x) = sqrt(4 - x^2) y g(x) = ln(x - 1)
abrir("ej01-f-y-g", w = 5, h = 6.4)
par(mfrow = c(2, 1))
curve(sqrt(4 - x^2), -2, 2, n = 400, lwd = 2, col = azul, asp = 1,
      xlim = c(-2.5, 2.5), ylim = c(-0.5, 2.5), xlab = "x", ylab = "f(x)")
ejes(); grid(col = "gray90")
abline(h = sqrt(3), lty = 2, col = gris)
points(c(-1, 1), c(sqrt(3), sqrt(3)), pch = 19, col = rojo)
text(0, sqrt(3) + 0.2, "f(-1) = f(1)", cex = 0.8, col = rojo)
curve(log(x - 1), 1.001, 6, n = 400, lwd = 2, col = azul,
      xlim = c(0, 6), ylim = c(-4, 2), xlab = "x", ylab = "g(x)")
ejes(); grid(col = "gray90")
abline(v = 1, lty = 2, col = rojo)
text(1.2, -3.5, "x = 1", pos = 4, cex = 0.8, col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej 2: parabola
abrir("ej02-parabola")
curve(x^2 - 4*x + 3, -0.8, 4.8, lwd = 2, col = azul, xlab = "x", ylab = "f(x)",
      ylim = c(-1.5, 7))
ejes(); grid(col = "gray90")
abline(v = 2, lty = 2, col = gris)
points(c(1, 3), c(0, 0), pch = 19, col = rojo)
points(2, -1, pch = 19, col = verde)
points(0, 3, pch = 19)
text(2, -1, "V(2, -1)", pos = 4, cex = 0.85)
text(c(1, 3), c(0, 0), c("x = 1", "x = 3"), pos = c(2, 4), cex = 0.85)
text(0, 3, "(0, 3)", pos = 4, cex = 0.85)
dev.off()

# ---------------------------------------------------------------
# Ej 3: composiciones
abrir("ej03-composiciones", w = 5, h = 6.4)
par(mfrow = c(2, 1))
fg <- function(x) sqrt((2*x - 1)/(x - 1))
plot(NA, xlim = c(-4, 5), ylim = c(0, 4), xlab = "x", ylab = "f(g(x))")
grid(col = "gray90"); ejes()
curve(fg(x), -4, 0.5, add = TRUE, lwd = 2, col = azul, n = 400)
curve(fg(x), 1.01, 5, add = TRUE, lwd = 2, col = azul, n = 400)
abline(v = 1, lty = 2, col = rojo); points(0.5, 0, pch = 19, col = azul)
rect(0.5, -1, 1, 5, col = adjustcolor(rojo, 0.12), border = NA)
text(0.75, 3.6, "fuera\ndel dominio", cex = 0.7, col = rojo)
gf <- function(x) 1/(sqrt(x + 2) - 1)
plot(NA, xlim = c(-2, 6), ylim = c(-6, 6), xlab = "x", ylab = "g(f(x))")
grid(col = "gray90"); ejes()
curve(gf(x), -2, -1.01, add = TRUE, lwd = 2, col = azul, n = 400)
curve(gf(x), -0.99, 6, add = TRUE, lwd = 2, col = azul, n = 400)
abline(v = -1, lty = 2, col = rojo); points(-2, -1, pch = 19, col = azul)
dev.off()

# ---------------------------------------------------------------
# Ej 4: x^3 + x y su inversa (reflejo respecto a y = x)
abrir("ej04-inversa", w = 5, h = 4.6)
t <- seq(-1.6, 1.6, length.out = 400)
plot(t, t^3 + t, type = "l", lwd = 2, col = azul, asp = 1,
     xlim = c(-5, 5), ylim = c(-5, 5), xlab = "x", ylab = "y")
grid(col = "gray90"); ejes()
lines(t^3 + t, t, lwd = 2, col = rojo, lty = 2)
abline(0, 1, col = gris, lty = 3)
legend("topleft", c(expression(f(x) == x^3 + x), expression(f^{-1}), "y = x"),
       col = c(azul, rojo, gris), lty = c(1, 2, 3), lwd = c(2, 2, 1), bg = "white", cex = 0.85)
dev.off()

# ---------------------------------------------------------------
# Ej 5: sigmoide, tanh y SiLU
abrir("ej05-activaciones", w = 6.5, h = 4)
s <- function(x) 1/(1 + exp(-x))
curve(s(x), -6, 6, lwd = 2, col = azul, ylim = c(-1.2, 6), xlab = "x", ylab = "y")
grid(col = "gray90"); ejes()
curve(tanh(x), -6, 6, lwd = 2, col = rojo, add = TRUE)
curve(x*s(x), -6, 6, lwd = 2, col = verde, lty = 2, add = TRUE)
abline(h = c(-1, 1), lty = 3, col = gris)
legend("topleft", c("s(x) = sigmoide", "t(x) = tanh(x)", "w(x) = x s(x)"),
       col = c(azul, rojo, verde), lty = c(1, 1, 2), lwd = 2, bg = "white", cex = 0.85)
dev.off()

# ---------------------------------------------------------------
# Ej 6 y 8: (x^2 - 4)/(x - 2) con el "hueco" en x = 2
abrir("ej06-hueco", w = 5, h = 3.6)
curve(x + 2, -1, 5, lwd = 2, col = azul, xlab = "x", ylab = "y", ylim = c(0, 7))
grid(col = "gray90"); ejes()
points(2, 4, pch = 21, bg = "white", col = azul, cex = 1.4, lwd = 2)
segments(2, 0, 2, 4, lty = 3, col = gris); segments(-1, 4, 2, 4, lty = 3, col = gris)
text(2, 4, "  (2, 4) no pertenece\n  a la grafica", pos = 4, cex = 0.75)
dev.off()

# ---------------------------------------------------------------
# Ej 7: funcion a trozos
abrir("ej07-trozos", w = 5, h = 3.6)
plot(NA, xlim = c(-1, 3.5), ylim = c(0, 3), xlab = "x", ylab = "f(x)")
grid(col = "gray90"); ejes()
segments(-1, 0, 1, 2, lwd = 2, col = azul)
segments(1, 2, 3.5, -0.5, lwd = 2, col = rojo)
points(1, 2, pch = 19)
legend("topright", c("x + 1  (x < 1)", "3 - x  (x >= 1)"), col = c(azul, rojo),
       lwd = 2, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej 9: epsilon-delta
abrir("ej09-epsilon-delta", w = 5.5, h = 4)
eps <- 1; del <- eps/2
plot(NA, xlim = c(1.5, 4.5), ylim = c(2, 8), xlab = "x", ylab = "y")
rect(-10, 5 - eps, 10, 5 + eps, col = adjustcolor(rojo, 0.12), border = NA)
rect(3 - del, -10, 3 + del, 20, col = adjustcolor(azul, 0.15), border = NA)
curve(2*x - 1, 1.5, 4.5, add = TRUE, lwd = 2)
abline(h = c(5 - eps, 5 + eps), lty = 2, col = rojo)
abline(v = c(3 - del, 3 + del), lty = 2, col = azul)
points(3, 5, pch = 19)
text(1.6, 5 + eps, expression(5 + epsilon), pos = 3, col = rojo, cex = 0.85)
text(1.6, 5 - eps, expression(5 - epsilon), pos = 1, col = rojo, cex = 0.85)
text(3 - del, 2.3, expression(3 - delta), pos = 2, col = azul, cex = 0.85)
text(3 + del, 2.3, expression(3 + delta), pos = 4, col = azul, cex = 0.85)
dev.off()

# ---------------------------------------------------------------
# Ej 10: limites trigonometricos
abrir("ej10-trig", w = 5, h = 6.4)
par(mfrow = c(2, 1))
curve(sin(3*x)/x, -3, 3, n = 600, lwd = 2, col = azul, xlab = "x", ylab = "",
      main = "sin(3x) / x", cex.main = 0.9, font.main = 1)
grid(col = "gray90"); ejes(); points(0, 3, pch = 21, bg = "white", col = azul, lwd = 2)
curve((1 - cos(x))/x^2, -6, 6, n = 600, lwd = 2, col = rojo, xlab = "x", ylab = "",
      main = expression((1 - cos ~ x) / x^2), cex.main = 0.9, ylim = c(0, 0.55))
grid(col = "gray90"); ejes(); points(0, 0.5, pch = 21, bg = "white", col = rojo, lwd = 2)
dev.off()

# ---------------------------------------------------------------
# Ej 11: funcion signo
abrir("ej11-signo", w = 5, h = 3.4)
plot(NA, xlim = c(-3, 3), ylim = c(-1.6, 1.6), xlab = "x", ylab = "|x| / x")
grid(col = "gray90"); ejes()
segments(-3, -1, 0, -1, lwd = 2, col = azul); segments(0, 1, 3, 1, lwd = 2, col = azul)
points(c(0, 0), c(-1, 1), pch = 21, bg = "white", col = azul, cex = 1.3, lwd = 2)
arrows(0.25, -0.9, 0.25, 0.9, code = 3, length = 0.07, col = rojo)
text(0.3, 0, "salto = 2", pos = 4, col = rojo, cex = 0.85)
dev.off()

# ---------------------------------------------------------------
# Ej 12: secantes que se acercan a la tangente (a = 1)
abrir("ej12-secantes", w = 5.5, h = 4)
f12 <- function(x) x^2 + 2*x
curve(f12(x), -0.5, 3.2, lwd = 2, xlab = "x", ylab = "f(x)", ylim = c(-2, 16))
grid(col = "gray90")
a <- 1; hs <- c(2, 1, 0.5)
cols <- c("gray70", "gray55", "gray40")
for (i in seq_along(hs)) {
  m <- (f12(a + hs[i]) - f12(a))/hs[i]
  abline(a = f12(a) - m*a, b = m, col = cols[i], lty = 2)
  points(a + hs[i], f12(a + hs[i]), pch = 19, cex = 0.7, col = cols[i])
}
abline(a = f12(a) - 4*a, b = 4, col = rojo, lwd = 2)
points(a, f12(a), pch = 19, col = rojo)
legend("topleft", c("secantes (h = 2; 1; 0,5)", "tangente: pendiente 2a + 2 = 4"),
       col = c("gray50", rojo), lty = c(2, 1), lwd = c(1, 2), bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej 14: x^4 - 3x^2 + 2 con criticos y tangente en x = 1
abrir("ej14-criticos", w = 6, h = 4)
f14 <- function(x) x^4 - 3*x^2 + 2
curve(f14(x), -2, 2, lwd = 2, col = azul, xlab = "x", ylab = "f(x)", ylim = c(-1, 4))
grid(col = "gray90"); ejes()
r <- sqrt(1.5)
points(c(-r, 0, r), f14(c(-r, 0, r)), pch = 19, col = c(verde, rojo, verde))
curve(-2*x + 2, -0.2, 2, add = TRUE, col = rojo, lty = 2, lwd = 1.5)
points(1, 0, pch = 4, lwd = 2)
text(0, 2, "max. local", pos = 3, cex = 0.8)
text(c(-r, r), -0.25, "min.", pos = 1, cex = 0.8)
text(1.3, -0.9, "tangente y = -2x + 2", cex = 0.75, col = rojo)
dev.off()

# ---------------------------------------------------------------
# Ej 15: diferencias finitas
f15 <- function(x) x^4 - 3*x^2 + 2
h <- 10^-(1:4)
adel <- (f15(1 + h) - f15(1))/h
cent <- (f15(1 + h) - f15(1 - h))/(2*h)
abrir("ej15-errores", w = 5.5, h = 4)
plot(h, abs(adel + 2), log = "xy", type = "b", pch = 19, lwd = 2, col = azul,
     ylim = c(1e-8, 1), xlab = "h", ylab = "error absoluto", xaxt = "n", yaxt = "n")
axis(1, at = h, labels = expression(10^-1, 10^-2, 10^-3, 10^-4))
axis(2, at = 10^-(0:8), labels = parse(text = paste0("10^", -(0:8))), cex.axis = 0.7)
grid(col = "gray90", equilogs = FALSE)
lines(h, abs(cent + 2), type = "b", pch = 15, lwd = 2, col = rojo)
legend("bottomright", c("adelante  ~ 3h", expression(centrada ~ "~" ~ 4*h^2)),
       col = c(azul, rojo), pch = c(19, 15), lwd = 2, bg = "white", cex = 0.85)
dev.off()

# ---------------------------------------------------------------
# Ej 16: f, f' y f''
abrir("ej16-f-fp-fpp", w = 6, h = 4)
curve(x^3 - 3*x, -2.3, 2.3, lwd = 2, col = azul, xlab = "x", ylab = "", ylim = c(-6, 8))
grid(col = "gray90"); ejes()
curve(3*x^2 - 3, add = TRUE, lwd = 1.5, lty = 2, col = rojo)
curve(6*x, add = TRUE, lwd = 1.5, lty = 3, col = verde)
points(c(-1, 1, 0), c(2, -2, 0), pch = 19)
legend("topleft", c(expression(f(x) == x^3 - 3*x), "f'(x)", "f''(x)"), col = c(azul, rojo, verde),
       lty = 1:3, lwd = 2, bg = "white", cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej 17: aproximacion lineal de sqrt(x) en x = 4
abrir("ej17-lineal", w = 5, h = 6.4)
par(mfrow = c(2, 1))
curve(sqrt(x), 0, 9, lwd = 2, col = azul, xlab = "x", ylab = "y")
grid(col = "gray90")
curve(2 + (x - 4)/4, 0, 9, add = TRUE, col = rojo, lty = 2, lwd = 1.5)
points(4, 2, pch = 19)
rect(3.9, 1.97, 4.2, 2.06, border = gris)
legend("topleft", c(expression(sqrt(x)), "L(x) = 2 + (x - 4)/4"), col = c(azul, rojo),
       lty = 1:2, lwd = 2, bg = "white", cex = 0.8)
curve(sqrt(x), 3.9, 4.2, lwd = 2, col = azul, xlab = "x", ylab = "y (zoom)")
grid(col = "gray90")
curve(2 + (x - 4)/4, add = TRUE, col = rojo, lty = 2, lwd = 1.5)
points(c(4.1, 4.1), c(sqrt(4.1), 2.025), pch = c(19, 17), col = c(azul, rojo))
dev.off()

# ---------------------------------------------------------------
# Funcion auxiliar para curvas de nivel
nivel <- function(f, xr, yr, n = 200, nlev = 15, ...) {
  xs <- seq(xr[1], xr[2], length.out = n); ys <- seq(yr[1], yr[2], length.out = n)
  z <- outer(xs, ys, f)
  contour(xs, ys, z, nlevels = nlev, col = gris, labcex = 0.6, asp = 1, ...)
}

# Ej 18 y 19: curvas de nivel con gradiente
abrir("ej19-gradiente", w = 5.5, h = 5)
nivel(function(x, y) x^2 + 2*y^2, c(-2.5, 2.5), c(-2.5, 2.5), xlab = "x", ylab = "y")
points(1, -1, pch = 19)
arrows(1, -1, 1 + 2/sqrt(20)*1.2, -1 - 4/sqrt(20)*1.2, col = rojo, lwd = 2, length = 0.1)
arrows(1, -1, 1 + 0.6*1.2, -1 + 0.8*1.2, col = azul, lwd = 2, length = 0.1)
legend("topleft", c("gradiente (2, -4)", "direccion u = (3, 4)/5"), col = c(rojo, azul),
       lwd = 2, bg = "white", cex = 0.8)
dev.off()

abrir("ej18-curvas-nivel", w = 5.5, h = 5)
nivel(function(x, y) x^2 + x*y + 3*y^2 - 2*x, c(-1.5, 3.5), c(-2, 1.5),
      xlab = "x", ylab = "y")
points(12/11, -2/11, pch = 19, col = rojo)
text(12/11, -2/11, "  minimo (12/11, -2/11)", pos = 4, cex = 0.75, col = rojo)
dev.off()

# Ej 20: minimo global
abrir("ej20-minimo", w = 5.5, h = 5)
nivel(function(x, y) x^2 + 2*y^2 - 2*x + 4*y, c(-1.5, 3.5), c(-3, 1),
      xlab = "x", ylab = "y")
points(1, -1, pch = 19, col = rojo); text(1, -1, "(1, -1)", pos = 4, col = rojo, cex = 0.8)
dev.off()

# Ej 21: superficies silla y x^4 + y^4
abrir("ej21-superficies", w = 4.6, h = 7)
par(mfrow = c(2, 1), mar = c(1, 1, 2, 1))
xs <- seq(-1, 1, length.out = 35)
pal <- colorRampPalette(c("#dff4f7", "#097289", azul))(20)
z1 <- outer(xs, xs, function(x, y) x^2 - y^2)
zf <- (z1[-1, -1] + z1[-1, -35] + z1[-35, -1] + z1[-35, -35])/4
persp(xs, xs, z1, theta = 35, phi = 25, col = pal[cut(zf, 20)], border = "gray40",
      lwd = 0.3, xlab = "x", ylab = "y", zlab = "f", main = "f = x^2 - y^2 (silla)",
      font.main = 1, cex.main = 0.9)
z2 <- outer(xs, xs, function(x, y) x^4 + y^4)
zf <- (z2[-1, -1] + z2[-1, -35] + z2[-35, -1] + z2[-35, -35])/4
persp(xs, xs, z2, theta = 35, phi = 25, col = pal[cut(zf, 20)], border = "gray40",
      lwd = 0.3, xlab = "x", ylab = "y", zlab = "g", main = "g = x^4 + y^4 (minimo)",
      font.main = 1, cex.main = 0.9)
dev.off()

# Ej 22: Taylor de orden 2 a lo largo de t = x + y
abrir("ej22-taylor", w = 5.5, h = 4)
curve(exp(x), -2, 2, lwd = 2, col = azul, xlab = "t = x + y", ylab = "")
grid(col = "gray90")
curve(1 + x + x^2/2, add = TRUE, lty = 2, lwd = 2, col = rojo)
curve(1 + x, add = TRUE, lty = 3, col = gris)
points(-0.1, exp(-0.1), pch = 19)
legend("topleft", c(expression(e^t), expression(1 + t + t^2/2), "1 + t (orden 1)"),
       col = c(azul, rojo, gris), lty = c(1, 2, 3), lwd = c(2, 2, 1), bg = "white", cex = 0.8)
dev.off()

# Ej 23: minimo de la cuadratica + trayectoria de descenso por gradiente
f23  <- function(x, y) 3*x^2 + 2*x*y + 2*y^2 - 6*x - 4*y
g23  <- function(p) c(6*p[1] + 2*p[2] - 6, 2*p[1] + 4*p[2] - 4)
p <- c(-1, 2); camino <- matrix(p, ncol = 2)
for (k in 1:25) { p <- p - 0.1*g23(p); camino <- rbind(camino, p) }
abrir("ej23-descenso", w = 5.5, h = 5)
nivel(f23, c(-1.5, 2.5), c(-1, 2.5), nlev = 18, xlab = "x", ylab = "y")
lines(camino, type = "b", pch = 20, cex = 0.6, col = azul)
points(0.8, 0.6, pch = 19, col = rojo, cex = 1.2)
text(0.8, 0.6, "(4/5, 3/5)", pos = 4, col = rojo, cex = 0.8)
dev.off()

# ---------------------------------------------------------------
# Ej 24: trapecio
abrir("ej24-trapecio", w = 5, h = 3.8)
curve(2*x + 1, -0.3, 2.5, lwd = 2, col = azul, xlab = "x", ylab = "y", ylim = c(0, 6.2))
grid(col = "gray90"); ejes()
polygon(c(0, 2, 2, 0), c(0, 0, 5, 1), col = adjustcolor(azul, 0.25), border = azul)
text(1, 1.4, "A = 6")
dev.off()

# Ej 25: sumas de Riemann
abrir("ej25-riemann", w = 5, h = 6.4)
par(mfrow = c(2, 1))
n <- 10; xi <- (1:n)/n
curve(x^2, 0, 1, lwd = 2, col = azul, xlab = "x", ylab = expression(x^2))
rect(xi - 1/n, 0, xi, xi^2, col = adjustcolor(rojo, 0.2), border = rojo)
curve(x^2, 0, 1, lwd = 2, col = azul, add = TRUE)
text(0.25, 0.8, sprintf("R_10 = %.3f", sum(xi^2)/n), cex = 0.85)
ns <- c(2, 5, 10, 20, 50, 100, 200, 500, 1000)
Rn <- (ns + 1)*(2*ns + 1)/(6*ns^2)
plot(ns, Rn, log = "x", type = "b", pch = 19, col = rojo, xlab = "n", ylab = expression(R[n]))
abline(h = 1/3, lty = 2, col = azul); grid(col = "gray90")
text(300, 1/3, "1/3", pos = 3, col = azul)
dev.off()

# Ej 29: x^(-p) para varios p
abrir("ej29-impropias", w = 5.5, h = 4)
curve(x^-0.5, 1, 10, lwd = 2, col = rojo, ylim = c(0, 1), xlab = "x", ylab = expression(x^-p))
curve(x^-1, 1, 10, lwd = 2, col = gris, add = TRUE)
curve(x^-2, 1, 10, lwd = 2, col = azul, add = TRUE)
grid(col = "gray90")
legend("topright", c("p = 1/2 (diverge)", "p = 1 (diverge)", "p = 2 (converge a 1)"),
       col = c(rojo, gris, azul), lwd = 2, bg = "white", cex = 0.8)
dev.off()

# Ej 31: region triangular D
abrir("ej31-region", w = 4.2, h = 4)
plot(NA, xlim = c(-0.1, 1.2), ylim = c(-0.1, 1.2), asp = 1, xlab = "x", ylab = "y")
grid(col = "gray90"); ejes()
polygon(c(0, 1, 1), c(0, 0, 1), col = adjustcolor(azul, 0.25), border = azul, lwd = 2)
segments(0.6, 0, 0.6, 0.6, col = rojo, lwd = 2)
text(0.6, 0.3, "y de 0 a x", pos = 2, cex = 0.75, col = rojo)
text(0.75, 0.15, "D", cex = 1.1)
text(0.5, 0.55, "y = x", srt = 45, cex = 0.8)
dev.off()

# Ej 34: proyeccion ortogonal
abrir("ej34-proyeccion", w = 4.5, h = 4.2)
plot(NA, xlim = c(-0.2, 2.5), ylim = c(-0.2, 2.3), asp = 1, xlab = "", ylab = "")
grid(col = "gray90"); ejes()
arrows(0, 0, 2, 1, lwd = 2, length = 0.1)
arrows(0, 0, 1, 2, lwd = 2, length = 0.1, col = azul)
arrows(0, 0, 0.8, 1.6, lwd = 3, length = 0.1, col = rojo)
segments(0.8, 1.6, 2, 1, lty = 2, col = verde, lwd = 2)
text(2, 1, "u = (2, 1)", pos = 4, cex = 0.8)
text(1, 2, "v = (1, 2)", pos = 3, cex = 0.8, col = azul)
text(0.55, 1.1, "proy", pos = 2, cex = 0.8, col = rojo)
text(1.45, 1.35, "r", pos = 4, cex = 0.9, col = verde)
dev.off()

# Ej 36: sistema como interseccion de rectas
abrir("ej36-rectas", w = 4.8, h = 4)
curve(5 - 2*x, -1, 4, lwd = 2, col = azul, xlab = "x", ylab = "y", ylim = c(-1, 4))
curve((7 - x)/3, add = TRUE, lwd = 2, col = rojo)
grid(col = "gray90"); ejes()
points(1.6, 1.8, pch = 19); text(1.6, 1.8, "(8/5, 9/5)", pos = 4, cex = 0.85)
legend("topright", c("2x + y = 5", "x + 3y = 7"), col = c(azul, rojo), lwd = 2,
       bg = "white", cex = 0.8)
dev.off()

# Ej 38: recta de minimos cuadrados y residuos
x38 <- c(0, 1, 2); y38 <- c(1, 2, 2)
ajuste <- lm(y38 ~ x38)
abrir("ej38-mco", w = 5, h = 3.8)
plot(x38, y38, pch = 19, xlim = c(-0.5, 2.5), ylim = c(0.5, 2.6), xlab = "x", ylab = "y")
grid(col = "gray90")
abline(ajuste, col = azul, lwd = 2)
segments(x38, y38, x38, fitted(ajuste), col = rojo, lwd = 2, lty = 2)
text(1.6, 1.25, expression(hat(y) == 7/6 + x/2), col = azul, cex = 0.9)
dev.off()

# Ej 39: superficie de SCR con un valle plano (infinitos minimos)
X39 <- cbind(c(1, 2, 3), c(2, 4, 6)); y39 <- c(1, 2, 2)
scr <- Vectorize(function(b1, b2) sum((y39 - X39 %*% c(b1, b2))^2))
abrir("ej39-valle", w = 5.5, h = 5)
nivel(scr, c(-2, 3), c(-1.5, 1.5), nlev = 14, xlab = expression(beta[1]),
      ylab = expression(beta[2]))
c1y <- sum(X39[, 1]*y39)/14
abline(a = c1y/2, b = -1/2, col = rojo, lwd = 2)
legend("topright", expression(beta[1] + 2*beta[2] == 11/14), col = rojo, lwd = 2,
       bg = "white", cex = 0.85)
dev.off()

cat("Listo:", length(list.files("img")), "graficos en img/\n")
