# Opción para poder visualizar todas las columnas
options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)
# Carga de los datos. Nos aseguramos de que las
# columnas string se carguen como factores
dataset <- read.csv("dataset.csv", stringsAsFactors=TRUE)
head(dataset, 5)
dim(dataset)
summary(dataset)
unique(as.vector.factor(dataset$Target))
dataset_limpio <- dataset[!(dataset$Target=="Enrolled"),]
dim(dataset_limpio)
# Label encoding
dataset_limpio$Target <- unclass(dataset_limpio$Target)
# Transformación a valor numérico
dataset_limpio$Target <- as.numeric(dataset_limpio$Target)
# Binarizamos al variable
dataset_limpio$Target[dataset_limpio$Target == 1] <- 0
dataset_limpio$Target[dataset_limpio$Target == 3] <- 1
head(dataset_limpio["Target"], 3)
columnas <- c(colnames(dataset)!="Target")
correlaciones <- round(cor(dataset_limpio[,columnas], dataset_limpio$Target), 2)
head(correlaciones[order(correlaciones,decreasing=TRUE),], 10)
columnas <- c("Curricular.units.2nd.sem..approved.", "Curricular.units.2nd.sem..grade.", "Curricular.units.1st.sem..approved.",
"Curricular.units.1st.sem..grade.", "Tuition.fees.up.to.date", "Scholarship.holder", "Curricular.units.2nd.sem..enrolled.",
"Curricular.units.1st.sem..enrolled.",	"Displaced")
correlaciones <- round(cor(dataset_limpio[,columnas], dataset_limpio[,columnas]), 2)
correlaciones
check_accuracy <- function(predictions, true_values) {
  count <- 0
  total <- length(true_values)
  for (x in 1:total) {
    if (predictions[x]==true_values[x]) {
      count <- count +1
    }
  }
  round(count/total, 3)
}
modelo <- glm(
  Target ~ Curricular.units.2nd.sem..approved. + Curricular.units.2nd.sem..grade. + Curricular.units.1st.sem..approved. +
Curricular.units.1st.sem..grade. + Tuition.fees.up.to.date + Scholarship.holder + Curricular.units.2nd.sem..enrolled.	+
Curricular.units.1st.sem..enrolled.	+ Displaced, data = dataset_limpio[1:3000,], family = "binomial")
summary(modelo)
test = dataset_limpio[3001:3630, c("Curricular.units.2nd.sem..approved.", "Curricular.units.2nd.sem..grade.", "Curricular.units.1st.sem..approved.",
"Curricular.units.1st.sem..grade.", "Tuition.fees.up.to.date", "Scholarship.holder", "Curricular.units.2nd.sem..enrolled.",
"Curricular.units.1st.sem..enrolled.",	"Displaced")]
test$Target_prob <- predict(modelo, newdata = test, type = "response")
test$Target_prediction[test$Target_prob >= 0.5] <- 1
test$Target_prediction[test$Target_prob < 0.5] <- 0
predictions <- test$Target_prediction
true_values <- dataset_limpio[3001:3630, "Target"]
check_accuracy(predictions, true_values)
