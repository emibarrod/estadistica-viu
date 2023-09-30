# instalación y carga de las librerías necesarias

install.packages('anytime')
install.packages('forecast', dependencies = TRUE)
library(anytime)
library(forecast)

# Preparación del dataset

gold_data <- read.csv(
  "/home/emibarrod/Documentos/Actividad2_estadistica/Gold Price.csv"
  )
gold_data <- subset(gold_data, select = c("Date","Price"))
View(gold_data)

# Creación de la serie temporal

timeserie <- ts(gold_data$Price, frequency = 365)
timeserie
plot.ts(timeserie, xlab="Tiempo", ylab = "Precio")

# Estadísticas de la serie temporal

length(timeserie)
start(timeserie)
end(timeserie)
head(timeserie)
tail(timeserie)
summary(timeserie)

# Descomposición

decomposition <- decompose(timeserie)
plot(decomposition, xlab="Tiempo")
plot(decomposition$trend)
plot(decomposition$seasonal)
plot(decomposition$random)

# Autocorrelación

autocorr <- acf(as.numeric(timeserie), 100, plot=FALSE)
plot(autocorr, main="Autocorrelación")
autocorr

# Auto ARIMA

fit_auto_arima <- auto.arima(timeserie)
fit_auto_arima
forecast(fit_auto_arima)
autoplot(forecast(fit_auto_arima))
