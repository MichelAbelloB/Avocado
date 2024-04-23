#Base de datos 

library(readr)
avocado_updated_2020 <- read_csv("C:/Users/Michel Abello/Downloads/master/act 1 Análisis de datos masivos para el negocio/avocado-updated-2020.csv")
View(avocado_updated_2020)

#Copia de datos

datos <- avocado_updated_2020

#(1) ¿De qué tipo de variables se compone mi base de datos? Realice un análisis exploratorio de las variables 
# numéricas (media, varianza, diagrama de cajas, etc) ¿Qué conclusiones se pueden extraer de la muestra de datos?

#Estructura de la base
str(datos)

#Variables de la base (media, varianza, diagrama de cajas, etc)
summary(datos)

#Validar si hay datos nulos
is.na(datos)

#Cambio del nombre de las variables (4046, 4225, 4770)
names(datos)[names(datos) == "4046"]<-"Tipo I"
names(datos)[names(datos) == "4225"]<-"Tipo II"
names(datos)[names(datos) == "4770"]<-"Tipo III"

summary(datos)

#Diagramas de cajas
boxplot(datos$xlarge_bags, main="Diagrama de Cajas", ylab="Valores")
boxplot(datos$large_bags, main="Diagrama de Cajas", ylab="Valores")
boxplot(datos$total_volume, main="Diagrama de Cajas", ylab="Valores")
boxplot(datos$average_price, main="Diagrama de Cajas", ylab="Valores")


#(2) Extraiga de la base de datos el precio de venta (la variable) de los aguacates orgánicos vendidos 
#en Albany y de Boston.

unique(datos$type)
unique(datos$geography)

solo_Boston <- datos[datos$geography == "Boston"& datos$type == "conventional",]
solo_Albany <- datos[datos$geography == "Albany"& datos$type == "conventional",]

Boston <- datos[datos$geography == "Boston"& datos$type == "organic",]
Albany <- datos[datos$geography == "Albany"& datos$type == "organic",]

Boston$price_sell <- Boston$average_price * Boston$total_volume
Albany$price_sell <- Albany$average_price * Albany$total_volume


#(3)	Como paso previo al modelado, calcule la covarianza y la matriz de correlación del precio de los 
#aguacates orgánicos, convencionales y su volumen de ventas. ¿Qué conclusiones se pueden extraer?


#--------------------------------------------------------------------------------------------------------------
#Media aritmetica aguacate organico
mean(datos$average_price)

#covarianza aguacate organico
datos_variables <- Boston[, c("average_price", "total_volume")]
cov(datos_variables)

cov(datos$average_price[datos$type == "organic"],datos$total_volume[datos$type == "organic"])
cov(datos$average_price[datos$type == "conventional"],datos$total_volume[datos$type == "conventional"])

#--------------------------------------------------------------------------------------------------------------

#Media aritmetica aguacate organico
mean(Boston$average_price)
mean(Albany$average_price)

#Media aritmetica aguacate conventional
mean(solo_Boston$average_price)
mean(solo_Albany$average_price)

#covarianza aguacate organico
Boston_variables <- Boston[, c("average_price", "total_volume")]
cov(Boston_variables)
Albany_variables <- Albany[, c("average_price", "total_volume")]
cov(Albany_variables)

#covarianza aguacate conventional
solo_Boston_variables <- solo_Boston[, c("average_price", "total_volume")]
cov(solo_Boston_variables)
solo_Albany_variables <- solo_Albany[, c("average_price", "total_volume")]
cov(solo_Albany_variables)

#correacion aguacate organico
cor(Boston_variables)
cor(Albany_variables)

#correacion aguacate organico
cor(solo_Boston_variables)
cor(solo_Albany_variables)

correlacion_organico <- cor(Boston$average_price, Boston$total_volume)
correlacion_convencional <- cor(solo_Boston$average_price, solo_Boston$total_volume)

Albany_correlacion_organico <- cor(Albany$average_price, Albany$total_volume)
Albany_correlacion_convencional <- cor(solo_Albany$average_price, solo_Albany$total_volume)

#(4) Determine la posible relación existente entre dichos precios y su volumen de ventas. Si tomáramos
#logaritmos, ¿Cómo sería dicha relación?

# Boston organico - logaritmo precios y volumen de ventas
log_average_price <- log(Boston$average_price)
log_total_volume <- log(Boston$total_volume)

log(Boston_variables)

# Albany organico - logaritmo precios y volumen de ventas
Albany_log_average_price <- log(Albany$average_price)
Albany_log_total_volume <- log(Albany$total_volume)

log(Albany_variables)

# correrlacion si tomaramos logaritmos (organica)

#Boston
correlacion_log_organico_boston <- cor(log_average_price, log_total_volume)

print("Correlación con logaritmos de precios orgánicos:")
print(correlacion_log_organico_boston)

#Albany
correlacion_log_organico_Albany <- cor(Albany_log_average_price, Albany_log_total_volume)

print("Correlación con logaritmos de precios orgánicos:")
print(correlacion_log_organico_Albany)


# Boston convencional - logaritmo precios y volumen de ventas
log_average_price_con <- log(solo_Boston$average_price)
log_total_volume_con <- log(solo_Boston$total_volume)

log(solo_Boston_variables)

# Albany convencional - logaritmo precios y volumen de ventas
Albany_log_average_price_con <- log(solo_Albany$average_price)
Albany_log_total_volume_con <- log(solo_Albany$total_volume)

log(solo_Albany_variables)

# correrlacion si tomaramos logaritmos (convencional)

#Boston
correlacion_log_organico_boston_con <- cor(log_average_price_con, log_total_volume_con)

print("Correlación con logaritmos de precios convencional:")
print(correlacion_log_organico_boston_con)

#Albany
correlacion_log_organico_Albany_con <- cor(Albany_log_average_price_con, Albany_log_total_volume_con)

print("Correlación con logaritmos de precios convencional:")
print(correlacion_log_organico_Albany_con)

#(5) Realice una predicción de precio de venta de los aguacates orgánicos vendidos en Albany a 3 meses. 

max(datos$date)
min(datos$date)

max(Albany$average_price)
min(Albany$average_price)

install.packages('forecast')
library(forecast)

Albany_Organic <- datos$average_price[datos$type == "organic" & datos$geography == "Albany"]
Albany_Organic_st <- ts(Albany_Organic, start = c(2015,1), frequency = 52)
plot(Albany_Organic_st)

Modelo_Albany_Organic <- auto.arima(Albany_Organic_st)
forecast_Albany_organic <- forecast(Modelo_Albany_Organic,12)
plot(forecast_Albany_organic, main = 'Forecast - predicción de precio de venta de los aguacates orgánicos', xlab = 'Date', ylab = 'Precio de venta')


