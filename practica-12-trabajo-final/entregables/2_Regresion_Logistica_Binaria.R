# Estadistica en Ciencia de Datos - Trabajo final
# Ejercicio: Regresion logistica binaria
# Base de datos: heart.csv (UCI)          Autor: Miguel Deza
# Ejecutar en la carpeta que contiene heart.csv:  Rscript 2_Regresion_Logistica_Binaria.R
# Paquetes: lmtest, pROC. Los graficos se guardan en img/ como PDF.
# Cada bloque "# >> nombre" ... "# <<" corresponde a una parte del informe 2_Regresion_Logistica_Binaria.pdf.

suppressPackageStartupMessages({ library(lmtest); library(pROC) })
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


# >> log-datos
cor_df <- read.csv("heart.csv")
cor_df$enf  <- factor(cor_df$enfermedad_cardiaca, levels = c("No", "Si"))
cor_df$sexo <- factor(cor_df$sexo, levels = 0:1, labels = c("mujer", "hombre"))
cor_df$angina_ejercicio <- factor(cor_df$angina_ejercicio, levels = 0:1,
                                  labels = c("no", "si"))
table(cor_df$enf)
# <<

# >> log-justifica
num <- c("edad", "colesterol", "presion_reposo", "frecuencia_cardiaca_max", "depresion_st")
t(sapply(num, function(v) {
  x <- cor_df[[v]]
  c(media_No = mean(x[cor_df$enf == "No"]), media_Si = mean(x[cor_df$enf == "Si"]),
    p_t = t.test(x ~ cor_df$enf)$p.value)
}))
prop.table(table(cor_df$sexo, cor_df$enf), 1)
prop.table(table(cor_df$angina_ejercicio, cor_df$enf), 1)
c(p_sexo = chisq.test(table(cor_df$sexo, cor_df$enf))$p.value,
  p_angina = chisq.test(table(cor_df$angina_ejercicio, cor_df$enf))$p.value)
# <<

# >> log-ajuste
m_log <- glm(enf ~ edad + sexo + colesterol + presion_reposo +
               frecuencia_cardiaca_max + angina_ejercicio + depresion_st,
             family = binomial(link = "logit"), data = cor_df)
summary(m_log)
# <<

# >> log-or
or <- exp(cbind(OR = coef(m_log), confint(m_log)))   # IC por verosimilitud perfil
round(or, 4)
# <<

# >> log-lr
m_nulo <- glm(enf ~ 1, family = binomial, data = cor_df)
lrtest(m_nulo, m_log)
c(R2_McFadden = 1 - as.numeric(logLik(m_log) / logLik(m_nulo)))
# <<

# >> log-confusion
p_hat <- fitted(m_log)
pred  <- factor(ifelse(p_hat >= 0.5, "Si", "No"), levels = c("No", "Si"))
mc <- table(Real = cor_df$enf, Predicha = pred)
mc
VN <- mc["No", "No"]; FP <- mc["No", "Si"]; FN <- mc["Si", "No"]; VP <- mc["Si", "Si"]
c(Exactitud = (VP + VN) / sum(mc), Sensibilidad = VP / (VP + FN),
  Especificidad = VN / (VN + FP), Tasa_error = (FP + FN) / sum(mc))
# <<

# >> log-roc
roc_log <- roc(cor_df$enf, p_hat, levels = c("No", "Si"), direction = "<", quiet = TRUE)
auc(roc_log)
ci.auc(roc_log)
coords(roc_log, "best", best.method = "youden",
       ret = c("threshold", "sensitivity", "specificity"))
# <<

# >> log-roc-fig
abrir("log-roc", w = 5.5, h = 6.6)
par(mfrow = c(2, 1))
plot(1 - roc_log$specificities, roc_log$sensitivities, type = "l", lwd = 2, col = azul,
     xlab = "1 - Especificidad", ylab = "Sensibilidad", panel.first = grid(col = "gray90"))
abline(0, 1, lty = 2, col = gris)
mejor <- coords(roc_log, "best", ret = c("specificity", "sensitivity"))
points(1 - mejor$specificity, mejor$sensitivity, pch = 19, col = rojo)
titulo(sprintf("Curva ROC  -  AUC = %.3f", auc(roc_log)))
boxplot(p_hat ~ cor_df$enf, col = adjustcolor(c(teal, rojo), 0.5), border = c(azul, rojo),
        horizontal = TRUE, xlab = "Probabilidad estimada P(Y = 1)", ylab = "")
abline(v = 0.5, lty = 2, col = gris)
titulo("Probabilidades estimadas por clase real (umbral 0,5)")
dev.off()
# <<

# >> log-cv
set.seed(2026)
pliegue <- sample(rep(1:10, length.out = nrow(cor_df)))
p_cv <- numeric(nrow(cor_df))
for (k in 1:10) {
  fit <- update(m_log, data = cor_df[pliegue != k, ])
  p_cv[pliegue == k] <- predict(fit, cor_df[pliegue == k, ], type = "response")
}
c(AUC_cv = as.numeric(auc(roc(cor_df$enf, p_cv, levels = c("No", "Si"),
                              direction = "<", quiet = TRUE))),
  Exactitud_cv = mean((p_cv >= 0.5) == (cor_df$enf == "Si")))
# <<
