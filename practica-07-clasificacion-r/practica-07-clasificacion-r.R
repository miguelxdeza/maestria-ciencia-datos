# =============================================================
# Practica 7 - Algoritmos de Clasificacion en Lenguaje R
# Dataset: Obesity Levels (Kaggle, fatemehmehrparvar/obesity-levels)
# Personas de Mexico, Peru y Colombia - 2111 filas, 17 columnas
# Algoritmos: LDA, CART, kNN, SVM lineal, Random Forest
# =============================================================

set.seed(7)

# ---- 1. Cargar las bibliotecas/paquetes necesarias ----
library(caret)
library(MASS)          # LDA
library(rpart)          # CART
library(rpart.plot)
library(e1071)           # SVM (y soporte interno de caret)
library(kernlab)          # motor de SVM lineal usado por caret
library(randomForest)
library(corrplot)
library(ggplot2)

# ---- 2. Cargar el conjunto de datos (CSV, descargado de Kaggle) ----
datos <- read.csv("obesidad.csv", stringsAsFactors = TRUE)

niveles_obesidad <- c("Insufficient_Weight", "Normal_Weight",
                       "Overweight_Level_I", "Overweight_Level_II",
                       "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III")
datos$NObeyesdad <- factor(datos$NObeyesdad, levels = niveles_obesidad)

str(datos)
dim(datos)
head(datos)

# ---- 3. Analisis exploratorio de datos ----
summary(datos)

cat("\nBalance de clases:\n")
print(table(datos$NObeyesdad))
print(round(prop.table(table(datos$NObeyesdad)) * 100, 1))

cat("\nValores faltantes por columna:\n")
print(colSums(is.na(datos)))

numericas <- c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE")
cat("\nCorrelacion entre predictores numericos:\n")
print(round(cor(datos[, numericas]), 2))

# ---- 4. Visualizar el conjunto de datos ----
# Paleta secuencial (cBg -> cTeal -> cTealDeep) porque NObeyesdad es una
# variable ORDINAL (a mas nivel, mas obesidad), no categorica sin orden.
paleta_seq <- colorRampPalette(c("#EAF6F8", "#097289", "#064B5A"))(7)
names(paleta_seq) <- niveles_obesidad

png("img/grafico-balance-clases.png", width = 950, height = 650, res = 130)
print(
  ggplot(datos, aes(x = NObeyesdad, fill = NObeyesdad)) +
    geom_bar(width = 0.6) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, size = 3.6) +
    scale_fill_manual(values = paleta_seq) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
    labs(title = "Balance de la variable de clase (NObeyesdad)", x = NULL, y = "Numero de personas") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 30, hjust = 1))
)
dev.off()

png("img/grafico-boxplots-variables.png", width = 1100, height = 500, res = 130)
print(
  ggplot(datos, aes(x = NObeyesdad, y = Weight, fill = NObeyesdad)) +
    geom_boxplot() +
    scale_fill_manual(values = paleta_seq) +
    labs(title = "Peso (kg) por nivel de obesidad", x = NULL, y = "Peso (kg)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none", plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 30, hjust = 1))
)
dev.off()

m_cor <- cor(datos[, numericas])
png("img/grafico-correlacion.png", width = 750, height = 750, res = 130)
corrplot(m_cor, method = "color", type = "upper",
         tl.col = "#064B5A", tl.srt = 45, addCoef.col = "#01111E", number.cex = 0.8,
         col.lim = c(min(m_cor), 1),
         col = colorRampPalette(c("#F4F4F4", "#097289", "#064B5A"))(200))
dev.off()

# ---- 5. Preprocesar los datos ----
# No hay valores faltantes. Las columnas categoricas (Gender, CALC, FAVC,
# SCC, SMOKE, family_history_with_overweight, CAEC, MTRANS) ya quedaron
# como factor con read.csv(stringsAsFactors=TRUE); train() las convierte
# a variables dummy automaticamente via la formula. Centro/escalo las
# numericas en el propio train() porque kNN y SVM son sensibles a escala.
set.seed(7)
idx_train <- createDataPartition(datos$NObeyesdad, p = 0.80, list = FALSE)
train <- datos[idx_train, ]
test  <- datos[-idx_train, ]
cat("\nFilas train / test:\n"); print(c(train = nrow(train), test = nrow(test)))

# ---- 6. Ejecutar los algoritmos ----
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
metric  <- "Accuracy"
prep    <- c("center", "scale")

set.seed(7); fit.lda  <- train(NObeyesdad ~ ., data = train, method = "lda",       metric = metric, trControl = control, preProcess = prep)
set.seed(7); fit.cart <- train(NObeyesdad ~ ., data = train, method = "rpart",     metric = metric, trControl = control, tuneLength = 8)
set.seed(7); fit.knn  <- train(NObeyesdad ~ ., data = train, method = "knn",       metric = metric, trControl = control, preProcess = prep, tuneLength = 10)
set.seed(7); fit.svm  <- train(NObeyesdad ~ ., data = train, method = "svmLinear", metric = metric, trControl = control, preProcess = prep)
set.seed(7); fit.rf   <- train(NObeyesdad ~ ., data = train, method = "rf",        metric = metric, trControl = control)

# ---- 7. Evaluar los algoritmos ----
resultados <- resamples(list(LDA = fit.lda, CART = fit.cart, kNN = fit.knn,
                              SVM = fit.svm, RF = fit.rf))
cat("\nResumen de precision por validacion cruzada (10-fold x 3 repeticiones):\n")
print(summary(resultados))

png("img/grafico-comparacion-cv.png", width = 950, height = 600, res = 130)
print(bwplot(resultados, main = "Comparacion de algoritmos (Accuracy / Kappa, CV)",
             col = "#097289", fill = "#0FC5ED"))
dev.off()

pred.lda  <- predict(fit.lda, test)
pred.cart <- predict(fit.cart, test)
pred.knn  <- predict(fit.knn, test)
pred.svm  <- predict(fit.svm, test)
pred.rf   <- predict(fit.rf, test)

cm.lda  <- confusionMatrix(pred.lda,  test$NObeyesdad)
cm.cart <- confusionMatrix(pred.cart, test$NObeyesdad)
cm.knn  <- confusionMatrix(pred.knn,  test$NObeyesdad)
cm.svm  <- confusionMatrix(pred.svm,  test$NObeyesdad)
cm.rf   <- confusionMatrix(pred.rf,   test$NObeyesdad)

cat("\n--- LDA ---\n");  print(cm.lda)
cat("\n--- CART ---\n"); print(cm.cart)
cat("\n--- kNN ---\n");  print(cm.knn)
cat("\n--- SVM ---\n");  print(cm.svm)
cat("\n--- RF ---\n");   print(cm.rf)

tabla_comparativa <- data.frame(
  Modelo         = c("LDA", "CART", "kNN", "SVM lineal", "Random Forest"),
  Accuracy_CV    = round(c(max(fit.lda$results$Accuracy), max(fit.cart$results$Accuracy),
                            max(fit.knn$results$Accuracy), max(fit.svm$results$Accuracy),
                            max(fit.rf$results$Accuracy)), 3),
  Accuracy_test  = round(c(cm.lda$overall["Accuracy"], cm.cart$overall["Accuracy"],
                            cm.knn$overall["Accuracy"], cm.svm$overall["Accuracy"],
                            cm.rf$overall["Accuracy"]), 3),
  Kappa_test     = round(c(cm.lda$overall["Kappa"], cm.cart$overall["Kappa"],
                            cm.knn$overall["Kappa"], cm.svm$overall["Kappa"],
                            cm.rf$overall["Kappa"]), 3),
  Sensib_macro   = round(c(mean(cm.lda$byClass[,"Sensitivity"]), mean(cm.cart$byClass[,"Sensitivity"]),
                            mean(cm.knn$byClass[,"Sensitivity"]), mean(cm.svm$byClass[,"Sensitivity"]),
                            mean(cm.rf$byClass[,"Sensitivity"])), 3),
  Especif_macro  = round(c(mean(cm.lda$byClass[,"Specificity"]), mean(cm.cart$byClass[,"Specificity"]),
                            mean(cm.knn$byClass[,"Specificity"]), mean(cm.svm$byClass[,"Specificity"]),
                            mean(cm.rf$byClass[,"Specificity"])), 3)
)
cat("\nTabla comparativa final:\n")
print(tabla_comparativa)
write.csv(tabla_comparativa, "tabla-comparativa.csv", row.names = FALSE)

png("img/grafico-arbol-cart.png", width = 1300, height = 850, res = 130)
rpart.plot(fit.cart$finalModel, type = 2, extra = 104, cex = 0.6,
           box.palette = as.list(paleta_seq),
           shadow.col = "gray85", main = "Arbol CART final")
dev.off()

png("img/grafico-importancia-rf.png", width = 850, height = 650, res = 130)
imp <- importance(fit.rf$finalModel)
imp_df <- data.frame(Variable = rownames(imp), MeanDecreaseGini = imp[, "MeanDecreaseGini"])
imp_df <- imp_df[order(imp_df$MeanDecreaseGini), ]
imp_df$Variable <- factor(imp_df$Variable, levels = imp_df$Variable)
print(
  ggplot(imp_df, aes(x = Variable, y = MeanDecreaseGini)) +
    geom_segment(aes(xend = Variable, y = 0, yend = MeanDecreaseGini), col = "#097289", linewidth = 1) +
    geom_point(size = 3.5, col = "#064B5A") +
    coord_flip() +
    labs(title = "Importancia de variables - Random Forest", x = NULL, y = "Mean Decrease Gini") +
    theme_minimal(base_size = 11) + theme(plot.title = element_text(face = "bold"))
)
dev.off()

# ---- 8. Hacer algunas predicciones ----
set.seed(7)
muestra_idx <- sample(nrow(test), 6)
muestra <- test[muestra_idx, ]
pred_muestra <- predict(fit.rf, muestra)
tabla_muestra <- data.frame(Real = muestra$NObeyesdad, Prediccion = pred_muestra)
cat("\nPredicciones (Random Forest) sobre 6 personas del set de prueba:\n")
print(tabla_muestra)

persona_nueva <- data.frame(
  Age = 27, Gender = "Male", Height = 1.72, Weight = 88,
  CALC = "Sometimes", FAVC = "yes", FCVC = 2, NCP = 3, SCC = "no", SMOKE = "no",
  CH2O = 2, family_history_with_overweight = "yes", FAF = 1, TUE = 2,
  CAEC = "Sometimes", MTRANS = "Public_Transportation"
)
pred_nueva <- predict(fit.rf, persona_nueva)
prob_nueva <- predict(fit.rf, persona_nueva, type = "prob")
cat("\nPrediccion Random Forest para persona nueva:\n")
print(pred_nueva)
print(round(prob_nueva, 3))
