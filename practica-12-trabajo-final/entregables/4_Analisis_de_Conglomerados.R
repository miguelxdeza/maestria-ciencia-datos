# Estadistica en Ciencia de Datos - Trabajo final
# Ejercicio: Analisis de conglomerados: Ward.D2 y k-means
# Base de datos: heart.csv (UCI)          Autor: Miguel Deza
# Ejecutar en la carpeta que contiene heart.csv:  Rscript 4_Analisis_de_Conglomerados.R
# Paquetes: cluster. Los graficos se guardan en img/ como PDF.
# Cada bloque "# >> nombre" ... "# <<" corresponde a una parte del informe 4_Analisis_de_Conglomerados.pdf.

suppressPackageStartupMessages({ library(cluster) })
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


# >> clu-datos
cor_df <- read.csv("heart.csv")
cor_df$enf  <- factor(cor_df$enfermedad_cardiaca, levels = c("No", "Si"))
cor_df$sexo <- factor(cor_df$sexo, levels = 0:1, labels = c("mujer", "hombre"))
cor_df$angina_ejercicio <- factor(cor_df$angina_ejercicio, levels = 0:1,
                                  labels = c("no", "si"))
vclu <- c("edad", "presion_reposo", "colesterol", "frecuencia_cardiaca_max", "depresion_st")
Y <- cor_df[, vclu]
round(rbind(media = colMeans(Y), desv_est = apply(Y, 2, sd)), 2)
Z <- scale(Y)
round(rbind(media = colMeans(Z), desv_est = apply(Z, 2, sd)), 2)
# <<

# >> clu-dist
D <- dist(Z, method = "euclidean")
length(D)                              # 297 * 296 / 2 pares
round(as.matrix(D)[1:6, 1:6], 3)
summary(as.vector(D))
# <<

# >> clu-dist-fig
abrir("clu-dist", w = 5.5, h = 6.6)
par(mfrow = c(2, 1))
hist(as.vector(D), breaks = 40, col = adjustcolor(teal, 0.55), border = "white",
     main = "", xlab = "Distancia euclidiana (variables estandarizadas)", ylab = "Frecuencia")
titulo("Distribucion de las 43 956 distancias")
D_raw <- dist(Y)
plot(as.vector(D_raw), as.vector(D), pch = ".", col = adjustcolor(azul, 0.3),
     xlab = "Distancia sin estandarizar", ylab = "Distancia estandarizada",
     panel.first = grid(col = "gray90"))
titulo(sprintf("Sin estandarizar domina el colesterol: r = %.2f con |dif. colesterol|",
               cor(as.vector(D_raw), as.vector(dist(Y$colesterol)))))
dev.off()
# <<

# >> clu-ward
hc <- hclust(D, method = "ward.D2")
alturas <- rev(hc$height)[1:8]
data.frame(k = 2:9, altura_fusion = round(alturas, 2),
           salto = round(c(-diff(alturas), NA), 2))
sil_hc <- sapply(2:6, function(k) mean(silhouette(cutree(hc, k), D)[, 3]))
round(setNames(sil_hc, paste0("k=", 2:6)), 3)
# <<

# >> clu-dendro
abrir("clu-dendro", w = 6, h = 6.6)
par(mfrow = c(2, 1))
plot(hc, labels = FALSE, hang = -1, main = "", xlab = "", sub = "", ylab = "Altura (Ward.D2)")
rect.hclust(hc, k = 3, border = rojo)
titulo("Dendrograma Ward.D2 con corte en k = 3")
plot(2:9, alturas, type = "b", pch = 19, col = azul, lwd = 2, xlab = "Numero de grupos k",
     ylab = "Altura de la fusion", panel.first = grid(col = "gray90"))
points(3, alturas[2], pch = 19, cex = 1.6, col = rojo)
titulo("Altura de fusion: el codo esta en k = 3")
dev.off()
# <<

# >> clu-perfil-hc
K <- 3
g_hc <- cutree(hc, k = K)
table(g_hc)
perfil <- function(g) {
  t(sapply(split(cor_df, g), function(d) c(n = nrow(d), colMeans(d[, vclu]),
    pct_hombre = 100 * mean(d$sexo == "hombre"),
    pct_angina = 100 * mean(d$angina_ejercicio == "si"),
    pct_enfermo = 100 * mean(d$enf == "Si"))))
}
round(perfil(g_hc), 1)
# <<

# >> clu-kmeans
set.seed(2026)
km <- kmeans(Z, centers = K, nstart = 50, iter.max = 100)
# reetiqueto: el grupo j de k-means es el que mas se solapa con el grupo j de Ward
eq <- apply(table(km$cluster, g_hc), 1, which.max)
km$cluster <- eq[km$cluster]; km$size <- km$size[order(eq)]
km$centers <- km$centers[order(eq), ]; rownames(km$centers) <- 1:K
km$size
round(km$centers, 2)                                      # centroides estandarizados
round(t(t(km$centers) * attr(Z, "scaled:scale") + attr(Z, "scaled:center")), 1)
c(R2_entre = km$betweenss / km$totss,
  silueta_km = mean(silhouette(km$cluster, D)[, 3]))
round(perfil(km$cluster), 1)
# <<

# >> clu-centroides
abrir("clu-centroides", w = 6, h = 4.6)
par(mar = c(9, 4, 1.8, 1))
matplot(t(km$centers), type = "b", pch = 19, lty = 1, lwd = 2, col = pal[1:K],
        xaxt = "n", xlab = "", ylab = "Centroide (unidades z)")
axis(1, at = 1:5, labels = vclu, las = 2, cex.axis = 0.75)
abline(h = 0, lty = 2, col = gris)
legend("topright", paste("Grupo", 1:K), col = pal[1:K], pch = 19, lwd = 2, cex = 0.75, bty = "n")
titulo("Perfil de los centroides k-means")
dev.off()
# <<

# >> clu-compara
tab_cmp <- table(Ward = g_hc, kmeans = km$cluster)
tab_cmp
ari <- function(a, b) {                       # indice de Rand ajustado
  t <- table(a, b); n <- sum(t); s <- sum(choose(t, 2))
  sa <- sum(choose(rowSums(t), 2)); sb <- sum(choose(colSums(t), 2))
  e <- sa * sb / choose(n, 2)
  (s - e) / ((sa + sb) / 2 - e)
}
c(ARI = ari(g_hc, km$cluster),
  acuerdo_max = sum(apply(tab_cmp, 1, max)) / sum(tab_cmp),
  silueta_ward = mean(silhouette(g_hc, D)[, 3]),
  silueta_kmeans = mean(silhouette(km$cluster, D)[, 3]))
# <<

# >> clu-plano
abrir("clu-plano", w = 5.5, h = 6.6)
par(mfrow = c(2, 1))
pc <- prcomp(Z)$x
plot(pc[, 1], pc[, 2], pch = 19, cex = 0.7, col = adjustcolor(pal[g_hc], 0.7),
     xlab = "CP1", ylab = "CP2", panel.first = grid(col = "gray90"))
legend("topright", paste("Grupo", 1:K), pch = 19, col = pal[1:K], cex = 0.7, bty = "n")
titulo("Ward.D2 (k = 3) en el plano de las dos primeras CP")
plot(pc[, 1], pc[, 2], pch = 19, cex = 0.7, col = adjustcolor(pal[km$cluster], 0.7),
     xlab = "CP1", ylab = "CP2", panel.first = grid(col = "gray90"))
legend("topright", paste("Grupo", 1:K), pch = 19, col = pal[1:K], cex = 0.7, bty = "n")
titulo("k-means (k = 3) en el mismo plano")
dev.off()
# <<

# >> clu-enfermedad
t_hc <- table(Ward = g_hc, enf = cor_df$enf); t_km <- table(kmeans = km$cluster, enf = cor_df$enf)
cbind(t_hc, pct_Si = round(100 * prop.table(t_hc, 1)[, "Si"], 1))
cbind(t_km, pct_Si = round(100 * prop.table(t_km, 1)[, "Si"], 1))
chisq.test(t_hc)
chisq.test(t_km)
# V de Cramer
sqrt(chisq.test(t_km)$statistic / (sum(t_km) * (min(dim(t_km)) - 1)))
# <<

# >> clu-enf-fig
abrir("clu-enfermedad", w = 6, h = 4)
pct <- rbind(Ward = prop.table(t_hc, 1)[, "Si"], kmeans = prop.table(t_km, 1)[, "Si"]) * 100
barplot(pct, beside = TRUE, names.arg = paste("Grupo", 1:K), col = c(teal, azul), border = NA,
        ylim = c(0, 100), ylab = "% con enfermedad cardiaca")
abline(h = 100 * mean(cor_df$enf == "Si"), lty = 2, col = rojo)
legend("topleft", c("Ward.D2", "k-means", "prevalencia global"), fill = c(teal, azul, NA),
       border = NA, lty = c(NA, NA, 2), col = c(NA, NA, rojo), cex = 0.75, bty = "n")
titulo("Prevalencia de enfermedad cardiaca por conglomerado")
dev.off()
# <<
