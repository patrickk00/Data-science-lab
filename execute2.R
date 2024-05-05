## ----------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(forecast)
library(dplyr)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(randomForest)
library(caret)
library(prophet)
library(rstanarm)
library(dplyr)
library(prophet)
library(ggplot2)
library(forecast)
library(zoo)
library(car)
library(forecast)
library(fastDummies)
library(forecast)
library(imputeTS)



## ----------------------------------------------------------------------------------------------------------------------------------
# Read the CSV file with explicit encoding
setwd("~/Desktop")
df<- read.csv("df_pulito.csv", header = TRUE)

#df <- read.csv("C:/Users/pc/Desktop/r004_data.csv", header = TRUE)#, fileEncoding = "ISO-8859-1")
df$date <- as.Date(df$date, format = "%Y-%m-%d")
start_date <- as.Date("2020-05-04")
end_date <- as.Date("2023-03-3")
filtered_post <- df %>%
  filter(date >= start_date & date <= end_date)
filtered_post$Giorno <- iconv(filtered_post$Giorno, from = "ISO-8859-1", to = "UTF-8")


## ----------------------------------------------------------------------------------------------------------------------------------
filtered_post



## ----------------------------------------------------------------------------------------------------------------------------------
ggplot(filtered_post, aes(x = date, y = lordo.totale)) +
  geom_line() +
  ggtitle("Lordo Totale Over Time") +
  xlab("Date") +
  ylab("Lordo Totale")
time_series <- ts(filtered_post$lordo.totale, start=c(2020, 5), frequency=365)
plot(time_series, main="Time Series of Lordo Totale", xlab="Year", ylab="Lordo Totale")
na_rows <- filtered_post[rowSums(is.na(filtered_post)) > 0,]
print(na_rows)
unique_days <- unique(filtered_post$Giorno)
print(unique_days)



## ----------------------------------------------------------------------------------------------------------------------------------
filtered_post$lordo.totale <- na.seadec(filtered_post$lordo.totale)
# Check if there are any NA values remaining after imputation
remaining_na <- sum(is.na(filtered_post$lordo.totale))
print(paste("Number of NA values after imputation:", remaining_na))


## ----------------------------------------------------------------------------------------------------------------------------------
na <- sum(is.na(filtered_post$lordo.totale))
na



## ----------------------------------------------------------------------------------------------------------------------------------

library(forecast)
library(ggplot2)

rolling_forecast_function <- function(data, model_type = "auto.arima", h = c(7), origins = 20, regressors = NULL, boxcox = FALSE, lambda = NULL) {
  
  if (is.data.frame(data)){
    obs <- nrow(data)
  } else {
    obs <- length(data)
  }
  print(paste("Number of observations: ", obs))
  

  result_collector <- data.frame(Horizon = numeric(0), RMSE = numeric(0), MAPE = numeric(0))




  for (j in h){
    rmse_array <- c()
    mape_array <- c()
    best_rmse <- Inf  
    best_iteration <- 0 
    print(paste("Horizon: ", j))
      for (i in 1:origins) {
      print(paste("iteration: ", i, "Observation: ", obs, " origins: ", origins, "j: ",j))
      train_size= obs+i-origins-j
      print(paste("Train size: ", train_size))
      if (!is.data.frame(data)){

        train_set <- data[1:train_size]
        test_set <- data[(train_size + 1):(obs + i - origins)]

      } else {
        train_set <- data[1:train_size,]
        test_set <- data[(train_size + 1):(obs + i - origins),]
      }
  
      if (!is.null(regressors)){
        train_regressor <- regressors[1:train_size,]
        test_regressor <- regressors[(train_size + 1):obs,]
      }
      
      if (model_type == "auto.arima") {
        if (is.null(regressors)) {
          set.seed(1) 
          model <- auto.arima(train_set, seasonal=TRUE)#, stepwise=FALSE, approximation=FALSE)
          } else {
            if (boxcox){
               lambda <- BoxCox.lambda(train_set, method="guerrero", lower=-1, upper=2)
              train_set <- BoxCox(train_set, lambda)
            }
            model <- auto.arima(train_set, seasonal = TRUE, stepwise = FALSE,lambda = lambda, approximation = FALSE, xreg = train_regressor)
        }
      } 
      else if (model_type == "prophet"){
        prophet_df <- data.frame(ds = train_set$date, y = train_set$lordo.totale)
        set.seed(13)
        model <- prophet(prophet_df)
        future <- make_future_dataframe(model, periods = nrow(test_set))
        forecasts <- predict(model, future, daily.seasonality=TRUE)# adding later daily.seasonality
  
      } 
      else if (model_type == "tbats") {
          model <- tbats(train_set, seasonal.periods=7)
      } 
      else if (model_type == "holt-winters") {
          model <- HoltWinters(ts(train_set, frequency=7), seasonal="additive")
      } 
      else if (model_type == "ets") {
          model <- ets(train_set, model = 'ANN')
      } 
      else if (model_type == "random.forest"){
        model <- randomForest(lordo.totale ~ Giorno + Weekend_New + Festivo_or_Weekend_New + lordo.totale_lag1 + Colore+lordo.totale_lag2 + lordo.totale_lag7, data=train_set)
      } 
      else if (model_type == "random.forest.lag") {
        model <- randomForest(lordo.totale ~ lag_1 + lag_7, data = train_set)

      }
      else {
        stop("Invalid model_type. Supported values are 'auto.arima', 'ets', 'tbats and 'holt-winters'")
      }
      
      if(model_type == 'auto.arima' || model_type == 'tbats' || model_type == 'ets' || model_type == 'holt-winters'){
        if(is.null(regressors) ){
       
          forecasts <- forecast(model, h=length(test_set))
          
        }else{
          print(paste("TEST REGRESSORS: ", test_regressor))

          print(paste("TEST SET: ", test_set))
          print(paste(model))
          print(paste("Number of regressors in the model: ", ncol(model$x)))
          print(paste("Number of regressors in the test data: ", ncol(test_regressor)))


          forecasts <- forecast(model, xreg=test_regressor, h=length(test_set))

           if(boxcox){
            forecasts$mean <- InvBoxCox(forecasts$mean, lambda)

          }
        }
      }
      else if (model_type == 'random.forest' || model_type == 'random.forest.lag') {
        forecasts <- predict(model, newdata=test_set)
  
      }
      #EVALUATION

      if (model_type == 'random.forest' || model_type == 'random.forest.lag'){
        rmse_test <- sqrt(mean((test_set$lordo.totale - forecasts)^2))
        mape_test <- mean(abs((test_set$lordo.totale - forecasts) / test_set$lordo.totale)) * 100
  
      }
      else if (model_type != 'prophet'){
        rmse_test <- sqrt(mean((test_set - forecasts$mean)^2))
        mape_test <- mean(abs((test_set - forecasts$mean) / test_set)) * 100
      } else {
        print("CHICHI")
        #print(paste("FORECAST:",forecasts))
        #print(paste("train: ",forecasts))
        rmse_test <- sqrt(mean((test_set$lordo.totale - forecasts$yhat[(nrow(train_set)+1):nrow(forecasts)])^2))
        mape_test <- mean(abs((test_set$lordo.totale - forecasts$yhat[(nrow(train_set) + 1):nrow(forecasts)]) / test_set$lordo.totale), na.rm = TRUE) * 100
        print(paste("RMSE on the test set:", rmse(test_set$lordo.totale, forecasts$yhat[(nrow(train)+1):nrow(forecasts)])))
        print(paste("MAPE on the test_set set:", MAPE(test_set$lordo.totale, forecasts$yhat[(nrow(train)+1):nrow(forecasts)])))

  
      }
      
      # Print the RMSE
      print(paste("RMSE on the test set:", rmse_test))
      print(paste("MAPE on the test set:", mape_test))
        
      rmse_array <- c(rmse_array, rmse_test)
      mape_array <- c(mape_array, mape_test)
  
      }
    
     
      result_collector[nrow(result_collector) + 1,] = c(j,mean(rmse_array), mean(mape_array))

      plot(forecasts)



  }



  return(list(result_collector, forecasts, model))
}




## ----------------------------------------------------------------------------------------------------------------------------------
filtered_post$Giorno <- gsub("lunedÃ¬", "lunedi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("martedÃ¬", "martedi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("mercoledÃ¬", "mercoledi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("giovedÃ¬", "giovedi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("venerdÃ¬", "venerdi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("sabato", "sabato", filtered_post$Giorno)
filtered_post$Giorno <- gsub("domenica", "domenica", filtered_post$Giorno)
filtered_tmp <- filtered_post %>%
  filter(ristorante == 'R003') 
vendite_ts <- ts(filtered_tmp$lordo.totale)
data <- vendite_ts
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
ets_model <- ets(train_vendite)
forecast_ets <- forecast(ets_model, h=length(test_vendite))
rmse_ets <- sqrt(mean((test_vendite - forecast_ets$mean)^2))
print(paste("RMSE for ETS:", rmse_ets))


## ----------------------------------------------------------------------------------------------------------------------------------


filtered_tmp <- filtered_post %>%
  filter(ristorante == 'R005' & lordo.totale != 0) 
data <- ts(filtered_tmp$lordo.totale, frequency = 1)
length(data)
result <- rolling_forecast_function(data, model_type = "ets", h = c(1,7,14,30,60), origins = 20, boxcox = TRUE)
result <- result[[1]]
forecasts <- result[[2]]
model <- result[[3]]

print(paste("Results: ", result))

 
result


## ----------------------------------------------------------------------------------------------------------------------------------
my_empty_list <- list()

# Append data using append() function
my_empty_list <- append(my_empty_list, "Data1")
my_empty_list <- append(my_empty_list, "Data2")
my_empty_list[0]

## ----------------------------------------------------------------------------------------------------------------------------------
# Preparazione dei dati per Prophet
prophet_df <- data.frame(ds = filtered_post$date, y = filtered_post$lordo.totale)
dummies_giorno <- model.matrix(~ Giorno - 1, data = filtered_post)
dummies_colore <- model.matrix(~ Colore - 1, data = filtered_post)
prophet_df <- cbind(prophet_df, dummies_giorno, dummies_colore)

filtered_post$Lockdown <- as.logical(filtered_post$Lockdown)
filtered_post$Weekend_New <- as.logical(filtered_post$Weekend_New)

prophet_df$Lockdown_1 <- as.integer(filtered_post$Lockdown)

prophet_df$Lockdown_0 <- as.integer(!filtered_post$Lockdown)
prophet_df$Weekend_1 <- as.integer(filtered_post$Weekend_New)
prophet_df$Weekend_0 <- as.integer(!filtered_post$Weekend_New)
h_values <- c(1, 7, 14)  # Orizzonti di previsione più corti
# Imposta la lunghezza del training set
initial_train_length <- 1010#floor(0.999 * nrow(prophet_df))

# Inizializzazione del dataframe per i risultati RMSE
results <- data.frame(ds=character(), RMSE=double())

for (t in (initial_train_length + 1):(nrow(prophet_df))) {
  print(paste("Iterationes: ", t))
  train_df <- prophet_df[1:t, ]
  test_df <- prophet_df[(t + 1):(t + 1), ]
  
  # Creazione e addestramento del modello Prophet
  model_prophet <- prophet(daily.seasonality = TRUE)
  regressors <- setdiff(names(prophet_df), c("ds", "y"))
  for (col in regressors) {
    model_prophet <- add_regressor(model_prophet, name = col)
  }
  model_prophet <- fit.prophet(model_prophet, train_df)
  
  # Previsione sul test set
  future <- make_future_dataframe(model_prophet, periods = 1)
  # Assicurati che future_extended abbia lo stesso numero di righe di future
  future_extended <- cbind(future, prophet_df[(t + 1):(t + 1), regressors])

  predictions <- predict(model_prophet, future_extended)
  rmse <- sqrt(mean((test_df$y - predictions$yhat)^2, na.rm = TRUE))

  # Salvataggio dei risultati RMSE
  results <- rbind(results, data.frame(ds=test_df$ds[1], RMSE=rmse))
}

# Stampa il dataframe dei risultati RMSE
print(results)

## ----------------------------------------------------------------------------------------------------------------------------------
filtered_post


## ----------------------------------------------------------------------------------------------------------------------------------
library(forecast)
# Non-seasonal Exponential Smoothing
es_model <- ets(train_vendite, lambda=TRUE,model="ANN") # "ANN" specifies a non-seasonal error, additive trend, no seasonality.
forecast_es <- forecast(es_model, h=length(test_vendite))
rmse_es <- sqrt(mean((test_vendite - forecast_es$mean)^2))
print(paste("RMSE for Non-seasonal Exponential Smoothing:", rmse_es))



## ----------------------------------------------------------------------------------------------------------------------------------
df_prophet <- data.frame(ds = as.Date(filtered_post$date), y = filtered_post$lordo.totale)
train_prophet <- df_prophet[1:train_size,]
test_prophet <- df_prophet[(train_size + 1):nrow(df_prophet),]
prophet_model <- prophet(train_prophet,daily.seasonality=TRUE)
forecast_prophet <- predict(prophet_model, test_prophet)
rmse_prophet <- sqrt(mean((test_prophet$y - forecast_prophet$yhat)^2))
print(paste("RMSE for Prophet:", rmse_prophet))


## ----------------------------------------------------------------------------------------------------------------------------------
filtered_post$Lockdown <- as.logical(filtered_post$Lockdown)
filtered_post$Weekend_New <- as.logical(filtered_post$Weekend_New)
filtered_post$Weekend_New


## ----------------------------------------------------------------------------------------------------------------------------------
prophet_df <- data.frame(ds = filtered_post$date, y = filtered_post$lordo.totale)
dummies_giorno <- model.matrix(~ Giorno - 1, data = filtered_post)
dummies_colore <- model.matrix(~ Colore - 1, data = filtered_post)
prophet_df <- cbind(prophet_df, dummies_giorno, dummies_colore)
prophet_df$Lockdown_1 <- as.integer(filtered_post$Lockdown)
prophet_df$Lockdown_0 <- as.integer(!filtered_post$Lockdown)
prophet_df$Weekend_1 <- as.integer(filtered_post$Weekend_New)
prophet_df$Weekend_0 <- as.integer(!filtered_post$Weekend_New)
train_size <- floor(0.8 * nrow(prophet_df))
train_df <- prophet_df[1:train_size, ]
test_df <- prophet_df[(train_size + 1):nrow(prophet_df), ]
model_prophet <- prophet(daily.seasonality = TRUE)
for (col in colnames(dummies_giorno)) {
    model_prophet <- add_regressor(model_prophet, name = col)
}
for (col in colnames(dummies_colore)) {
    model_prophet <- add_regressor(model_prophet, name = col)
}
model_prophet <- add_regressor(model_prophet, name = "Lockdown_1")
model_prophet <- add_regressor(model_prophet, name = "Lockdown_0")
model_prophet <- add_regressor(model_prophet, name = "Weekend_1")
model_prophet <- add_regressor(model_prophet, name = "Weekend_0")

# Fit the model on the training data
model_prophet <- fit.prophet(model_prophet, train_df)

# Predict on the test set
future <- data.frame(ds = test_df$ds,freq = 'm') #boh provo la moltiplicativa perchè nell'hot winters era andata bene
future <- cbind(future, dummies_giorno[(train_size + 1):nrow(prophet_df), ])
future <- cbind(future, dummies_colore[(train_size + 1):nrow(prophet_df), ])
future$Lockdown_1 <- test_df$Lockdown_1
future$Lockdown_0 <- test_df$Lockdown_0
future$Weekend_1 <- test_df$Weekend_1
future$Weekend_0 <- test_df$Weekend_0
predictions <- predict(model_prophet, future)
rmse <- sqrt(mean((test_df$y - predictions$yhat)^2))
print(paste("RMSE for Prophet:", rmse))
print(model_prophet$params$beta)



## ----------------------------------------------------------------------------------------------------------------------------------
result <- rolling_forecast_function(filtered_post, model_type = "prophet", h = c(7,14,23,70), origins = 20, )
result <- result[[1]]
forecasts <- result[[2]]
model <- result[[3]]

print(paste("Results: ", result))

 
result


## ----------------------------------------------------------------------------------------------------------------------------------

#MARKED DOWN
filtered_post$Giorno <- gsub("lunedÃ¬", "lunedi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("martedÃ¬", "martedi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("mercoledÃ¬", "mercoledi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("giovedÃ¬", "giovedi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("venerdÃ¬", "venerdi", filtered_post$Giorno)
filtered_post$Giorno <- gsub("sabato", "sabato", filtered_post$Giorno)
filtered_post$Giorno <- gsub("domenica", "domenica", filtered_post$Giorno)

#Create the dataframe for Prophet with 'ds' for dates and 'y' for values
prophet_df <- data.frame(ds = filtered_post$date, y = filtered_post$lordo.totale)

#Create dummy variables for 'Giorno'
dummies <- model.matrix(~ Giorno - 1, data = filtered_post)
prophet_df <- cbind(prophet_df, dummies)

train_size <- floor(0.8 * nrow(prophet_df))
prophet_train <- prophet_df[1:train_size, ]
prophet_test <- prophet_df[(train_size + 1):nrow(prophet_df), ]
model_prophet <- prophet(yearly.seasonality = TRUE)

#Add dummy variables for 'Giorno' to the model
for (col in colnames(dummies)) {
  model_prophet <- add_regressor(model_prophet, name = col)
}

#Fit the Prophet model
model_prophet <- fit.prophet(model_prophet, prophet_train)

#Prepare future dataframe
future <- make_future_dataframe(model_prophet, periods = nrow(prophet_test))

#Add dummy variables for 'Giorno' to the future dataframe
future_dummies <- model.matrix(~ Giorno - 1, data = filtered_post[(train_size + 1):nrow(filtered_post), ])
future <- cbind(future, future_dummies)
Forecast
forecasts_prophet <- predict(model_prophet, future)
rmse_prophet <- sqrt(mean((prophet_test$y - forecasts_prophet$yhat[(train_size + 1):nrow(prophet_df)])^2))
print(paste("RMSE for Prophet:", rmse_prophet))
plot(model_prophet, forecasts_prophet)



## ----------------------------------------------------------------------------------------------------------------------------------
summary(model_prophet)



## ----------------------------------------------------------------------------------------------------------------------------------
vendite_ts <- ts(filtered_post$lordo.totale,  frequency = (365.25/7))
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
hw_model <- HoltWinters(train_vendite,seasonal = "multiplicative" ,beta = FALSE, gamma = FALSE)
forecast_hw <- forecast(hw_model, h=length(test_vendite))
rmse_hw <- sqrt(mean((test_vendite - forecast_hw$mean)^2))
print(paste("RMSE for Holt-Winters with Additive Seasonality:", rmse_hw))



## ----------------------------------------------------------------------------------------------------------------------------------
result <- rolling_forecast_function(data, model_type = "holt-winters", h = c(7,14,23,70), origins = 20, regressors = NULL)

result <- result[[1]]
forecasts <- result[[2]]
model <- result[[3]]

print(paste("Results: ", result))

 
result


## ----------------------------------------------------------------------------------------------------------------------------------
unique(filtered_post$Giorno)
unique(filtered_post$Weekend)
unique(filtered_post$Festivo_or_Weekend)
unique(filtered_post$Festivo)
unique(filtered_post$Colore)
unique(filtered_post$Lockdown)



## ----------------------------------------------------------------------------------------------------------------------------------
head(filtered_post)


## ----------------------------------------------------------------------------------------------------------------------------------
library(randomForest)
filtered_post <- filtered_post %>%
  mutate(
    lordo.totale_lag1 = lag(lordo.totale, 1),
    lordo.totale_lag2 = lag(lordo.totale, 2),
    lordo.totale_lag7 = lag(lordo.totale, 7)
  )
#CREATE LAGS USED IN OTHER MODELS LATER
filtered_post$Giorno <- as.factor(filtered_post$Giorno)
filtered_post$Colore <- as.factor(filtered_post$Colore)
filtered_post <- na.omit(filtered_post)
train_size <- floor(0.8 * nrow(filtered_post))
train_df <- filtered_post[1:train_size, ]
test_df <- filtered_post[(train_size + 1):nrow(filtered_post), ]
set.seed(3)
rf_model <- randomForest(lordo.totale ~ Giorno + Weekend_New + Festivo_or_Weekend_New + lordo.totale_lag1 + Colore+lordo.totale_lag2 + lordo.totale_lag7, data=train_df)
print(rf_model)
predictions <- predict(rf_model, newdata=test_df)
importance(rf_model)
varImpPlot(rf_model)
rmse <- sqrt(mean((test_df$lordo.totale - predictions)^2))
print(rmse)


## ----------------------------------------------------------------------------------------------------------------------------------
result <- rolling_forecast_function(filtered_post, model_type = "random.forest", h = c(1,7,14,30,60), origins = 100, regressors = NULL)
result <- result[[1]]
forecasts <- result[[2]]
model <- result[[3]]

print(paste("Results: ", result))

 
result


## ----------------------------------------------------------------------------------------------------------------------------------
library(forecast)
filtered_post <- filtered_post %>%
  mutate(
    lordo.totale_lag1 = lag(lordo.totale, 1),
    lordo.totale_lag2 = lag(lordo.totale, 2),
    lordo.totale_lag7 = lag(lordo.totale, 7)
  )
train_size <- floor(0.8 * nrow(filtered_post))
train <- filtered_post[1:train_size, ]
test <- filtered_post[(train_size + 1):nrow(filtered_post), ]
set.seed(2304)
# Fit a Bayesian linear regression model with Giorno, multiple lags and possibly weekend
model_bayesian_with_lags <- stan_glm(lordo.totale ~ Giorno +Colore+ lordo.totale_lag1 + Lockdown+ rain + lordo.totale_lag7, 
                                     data = train)
summary(model_bayesian_with_lags)
predictions <- predict(model_bayesian_with_lags, newdata = test)
RMSE <- sqrt(mean((predictions - test$lordo.totale)^2))
print(RMSE)
residuals <- test$lordo.totale - predictions
ljung_box_test <- Box.test(residuals, type = "Ljung-Box")
print(ljung_box_test)


## ----------------------------------------------------------------------------------------------------------------------------------
library(forecast)
filtered_post <- filtered_post %>%
  mutate(
    lordo.totale_lag1 = lag(lordo.totale, 1),
    lordo.totale_lag2 = lag(lordo.totale, 2),
    lordo.totale_lag7 = lag(lordo.totale, 7)
  )
set.seed(1)
filtered_post$Giorno <- as.factor(filtered_post$Giorno)
filtered_post$Colore <- as.factor(filtered_post$Colore)
model_bayesian_with_lags <- stan_glm(lordo.totale ~ Giorno+ Colore+lordo.totale_lag1, 
                                     data = train)
summary(model_bayesian_with_lags)
predictions <- predict(model_bayesian_with_lags, newdata = test)
RMSE <- sqrt(mean((predictions - test$lordo.totale)^2))
print(RMSE)
residuals <- test$lordo.totale - predictions
ljung_box_test <- Box.test(residuals, type = "Ljung-Box")
print(ljung_box_test)



## ----------------------------------------------------------------------------------------------------------------------------------
filtered_post <- filtered_post %>%
  mutate(
    lordo.totale_lag1 = lag(lordo.totale, 1),
    lordo.totale_lag2 = lag(lordo.totale, 2),
    lordo.totale_lag7 = lag(lordo.totale, 7)
  )


## ----------------------------------------------------------------------------------------------------------------------------------
head(filtered_post)


## ----------------------------------------------------------------------------------------------------------------------------------
train_size <- floor(0.8 * nrow(filtered_post))
train <- filtered_post[1:train_size, ]
test <- filtered_post[(train_size + 1):nrow(filtered_post), ]
set.seed(2304)
# Fit a Bayesian linear regression model with Giorno, multiple lags and possibly weekend
model_bayesian_with_lags <- stan_glm(lordo.totale ~ Giorno +Colore+ lordo.totale_lag1 + Lockdown+ rain + lordo.totale_lag7, 
                                     data = train)
summary(model_bayesian_with_lags)
predictions <- predict(model_bayesian_with_lags, newdata = test)
RMSE <- sqrt(mean((predictions - test$lordo.totale)^2))
print(RMSE)
residuals <- test$lordo.totale - predictions
ljung_box_test <- Box.test(residuals, type = "Ljung-Box")
print(ljung_box_test)


## ----------------------------------------------------------------------------------------------------------------------------------
f <- filtered_post %>%
  filter(ristorante == 'R004') 
vendite_ts <- ts(f$lordo.totale , frequency = 365.25/7)
# Generate Fourier terms to capture seasonality. 
# K is the number of sine and cosine terms (the more terms, the more complex seasonality you can capture).
K <- 4
fourier_terms <- fourier(vendite_ts, K=K)
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
train_fourier <- fourier_terms[1:train_size,]
test_fourier <- fourier_terms[(train_size + 1):nrow(fourier_terms),]
fit <- auto.arima(train_vendite, xreg=train_fourier, seasonal=TRUE)
forecasted <- forecast(fit, xreg=test_fourier, h=length(test_vendite))
checkresiduals(fit)
rmse <- sqrt(mean((test_vendite - forecasted$mean)^2))
print(paste("RMSE for ARIMA with Fourier terms:", rmse))
summary(fit)


## ----------------------------------------------------------------------------------------------------------------------------------
#vendite_ts <- ts(filtered_post$lordo.totale, frequency = 365.25/7)
fourier_weekly <- fourier(ts(vendite_ts, frequency=7), K=3)
fourier_yearly <- fourier(ts(vendite_ts, frequency=365.25), K=15)
fourier_combined <- cbind(fourier_weekly, fourier_yearly)
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
train_fourier <- fourier_combined[1:train_size,]
test_fourier <- fourier_combined[(train_size + 1):nrow(fourier_combined),]
fit <- auto.arima(train_vendite, xreg=train_fourier, seasonal=FALSE)
checkresiduals(fit)
forecasted <- forecast(fit, xreg=test_fourier, h=length(test_vendite))
rmse <- sqrt(mean((test_vendite - forecasted$mean)^2))
print(paste("RMSE for ARIMA with Fourier terms:", rmse))



## ----------------------------------------------------------------------------------------------------------------------------------

#vendite_ts <- ts(filtered_post$lordo.totale, frequency = 365.25/7)
K <- 4
fourier_terms <- fourier(vendite_ts, K=K)
colore_dummies <- model.matrix(~ Colore - 1, data = f)[,-1]  # drop the first column
combined_xreg <- cbind(fourier_terms, colore_dummies)
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
train_regressors <- combined_xreg[1:train_size,]
test_regressors <- combined_xreg[(train_size + 1):nrow(combined_xreg),]
fit <- auto.arima(train_vendite, xreg=train_regressors, seasonal=TRUE)
forecasted <- forecast(fit, xreg=test_regressors, h=length(test_vendite))
checkresiduals(fit)
rmse <- sqrt(mean((test_vendite - forecasted$mean)^2))
print(paste("RMSE for ARIMA with Fourier terms and 'Colore' dummies:", rmse))
summary(fit)


length(fourier_terms)


## ----------------------------------------------------------------------------------------------------------------------------------
#vendite_ts <- ts(filtered_post$lordo.totale, frequency = 365.25/7)
K <- 4
fourier_terms <- fourier(vendite_ts, K=K)
colore_dummies <- model.matrix(~ Colore - 1, data = f)[,-1]  # drop the first column
giorno_dummies <- model.matrix(~ Giorno - 1, data = f)[,-1]  # drop the first column
combined_xreg <- cbind(fourier_terms, colore_dummies, giorno_dummies)
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
train_regressors <- combined_xreg[1:train_size,]
test_regressors <- combined_xreg[(train_size + 1):nrow(combined_xreg),]
fit <- auto.arima(train_vendite, xreg=train_regressors, seasonal=TRUE)
forecasted <- forecast(fit, xreg=test_regressors, h=length(test_vendite))
checkresiduals(fit)
rmse <- sqrt(mean((test_vendite - forecasted$mean)^2))
print(paste("RMSE for ARIMA with Fourier terms, 'Colore', and 'Giorno' dummies:", rmse))
summary(fit)



## ----------------------------------------------------------------------------------------------------------------------------------
# Create lagged variables
filtered_post <- filtered_post %>%
  filter(ristorante == 'R003') 
filtered_post$lag_1 <- lag(filtered_post$lordo.totale, 1)
filtered_post$lag_2 <- lag(filtered_post$lordo.totale, 2)
filtered_post$lag_7 <- lag(filtered_post$lordo.totale, 7)
filtered_post <- filtered_post[8:nrow(filtered_post), ]
colore_dummies <- model.matrix(~ Colore, data = filtered_post)[,-1]  # Drop first column
giorno_dummies <- model.matrix(~ Giorno, data = filtered_post)[,-1] 
combined_xreg <- cbind(colore_dummies, giorno_dummies, filtered_post$lag_1, filtered_post$lag_2, filtered_post$lag_7)
vendite_ts <- ts(filtered_post$lordo.totale, frequency = 1)
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
train_predictors <- combined_xreg[1:train_size, ]
test_predictors <- combined_xreg[(train_size + 1):nrow(combined_xreg), ]
fit <- auto.arima(train_vendite, xreg=train_predictors, seasonal=TRUE)
forecasted <- forecast(fit, xreg=test_predictors, h=length(test_vendite))
checkresiduals(fit)
rmse <- sqrt(mean((test_vendite - forecasted$mean)^2))
print(paste("RMSE for SARIMAX with 'Colore', 'Giorno', and Lagged Variables:", rmse))
summary(fit)



## ----------------------------------------------------------------------------------------------------------------------------------
filtered_post <- filtered_post %>%
    filter(lordo.totale != 0)
d <- filtered_post
d$lag_1 <- lag(d$lordo.totale, 1)
d$lag_2 <- lag(d$lordo.totale, 2)
d$lag_7 <- lag(d$lordo.totale, 7)
d <- d[8:nrow(d), ]
colore_dummies <- model.matrix(~ Colore, data = d)[,-1]  # Drop first column
giorno_dummies <- model.matrix(~ Giorno, data = d)[,-1] 
combined_xreg <- cbind(colore_dummies, giorno_dummies, d$lag_1, d$lag_2, d$lag_7)
vendite_ts <- ts(d$lordo.totale, frequency = 1)
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
train_predictors <- combined_xreg[1:train_size, ]
test_predictors <- combined_xreg[(train_size + 1):nrow(combined_xreg), ]
fit <- auto.arima(train_vendite, xreg=train_predictors, seasonal=TRUE)
forecasted <- forecast(fit, xreg=test_predictors, h=length(test_vendite))
rmse <- sqrt(mean((test_vendite - forecasted$mean)^2))
print(paste("RMSE for SARIMAX with 'Colore', 'Giorno', and Lagged Variables:", rmse))
mape_test_ets <- mean(abs((test_vendite - forecasted$mean) / test_vendite)) * 100

print(paste("MAPE on the test set:", mape_test_ets))


## ----------------------------------------------------------------------------------------------------------------------------------

# dum_day_df <- filtered_post[, c("date", "Giorno")]
# dum_day_df$Giorno <- as.factor(dum_day_df$Giorno)
# dum_day_df <- dummy_cols(dum_day_df, select_columns = c("Giorno"), remove_selected_columns = TRUE)
# 
# dum_day_df <- dum_day_df[ , !(names(dum_day_df) %in% c("date", "Giorno_lunedì"))]
# 
# data <- ts(filtered_post$lordo.totale, frequency = 1)

filtered_post <- filtered_post %>%
    filter(lordo.totale != 0)
d <- filtered_post
d$lag_1 <- lag(d$lordo.totale, 1)
d$lag_2 <- lag(d$lordo.totale, 2)
d$lag_7 <- lag(d$lordo.totale, 7)
d <- d[8:nrow(d), ]
colore_dummies <- model.matrix(~ Colore, data = d)[,-1]  # Drop first column
giorno_dummies <- model.matrix(~ Giorno, data = d)[,-1] 
combined_xreg <- cbind(colore_dummies, giorno_dummies, d$lag_1, d$lag_2, d$lag_7)


result <- rolling_forecast_function(d$lordo.totale, model_type = "auto.arima", h = c(7,14,30,60, 196), origins = 5, regressors = combined_xreg, boxcox = FALSE)
result <- result[[1]]
forecasts <- result[[2]]
model <- result[[3]]

print(paste("Results: ", result))

 
result


## ----------------------------------------------------------------------------------------------------------------------------------

data <- ts(filtered_post$lordo.totale, frequency = 1)

result <- rolling_forecast_function(data, model_type = "auto.arima", h = c(70), origins = 1, regressors = combined_xreg, boxcox = FALSE)
mean_rmse <- result[[1]]
mean_mape <- result[[2]]
forecasts <- result[[3]]
model <- result[[4]]

print(paste("Mean RMSE: ", mean_rmse))
print(paste("Mean MAPE: ", mean_mape))

 
plot(forecasts)


## ----------------------------------------------------------------------------------------------------------------------------------
plot(train)


## ----------------------------------------------------------------------------------------------------------------------------------
# Filter, add the dummy variable for 'Colore', and aggregate by ISO week
filtered_post <- df %>%
  filter(date >= start_date & date <= end_date & ristorante == "R004") %>%
  mutate(rosso_dummy = ifelse(Colore == "Rossa", 1, 0)) %>%
  mutate(week = lubridate::isoweek(date)) %>%
  group_by(week) %>%
  summarise(lordo.totale = mean(lordo.totale, na.rm = TRUE))


n <- length(filtered_post$lordo.totale)
train <- filtered_post$lordo.totale[1:(0.8*n)]
test <- filtered_post$lordo.totale[(0.8*n + 1):n]

# Fit a SARIMA(7,1,4)(0,1,0)[7] model
model_sarima <- auto.arima(train, seasonal = TRUE)

# Forecast the test data
forecasts <- forecast(model_sarima, h = length(test))
summary(model_sarima)
# Calculate RMSE for the test set
rmse_test <- sqrt(mean((test - forecasts$mean)^2))
print(paste("RMSE on the test set:", rmse_test))

# Plot residuals
residuals <- residuals(model_sarima)
plot(residuals, main="Residuals from SARIMA model with weekly aggregation")
checkresiduals(model_sarima)
# Perform Ljung-Box test
ljung_box_test <- Box.test(residuals, type = "Ljung-Box", lag=10)
print(ljung_box_test)

checkresiduals(model_sarima)



## ----------------------------------------------------------------------------------------------------------------------------------
plot(time_series, main="Time Series of Lordo Totale", xlab="Year", ylab="Lordo Totale")


## ----------------------------------------------------------------------------------------------------------------------------------


#df <- read.csv("C:/Users/pc/Desktop/r004_data.csv", header = TRUE)#, fileEncoding = "ISO-8859-1")


df$date <- as.Date(df$date, format = "%Y-%m-%d")
start_date <- as.Date("2020-05-04")
end_date <- as.Date("2023-03-3")
filtered_post <- df %>%
  filter(date >= start_date & date <= end_date)

ts_data <- ts(filtered_post$lordo.totale, frequency=365) 
decomposition <- stl(ts_data, s.window="periodic")
plot(decomposition)



## ----------------------------------------------------------------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)

# Weekly decomposition
ts_data_weekly <- ts(filtered_post$lordo.totale, start=c(2020, 125), frequency=7) # starting from the 125th day of 2020
decomposition_weekly <- stl(ts_data_weekly, s.window="periodic")

# Annual decomposition (for visualization, we'll just reuse the one you've already created)
ts_data_annual <- ts(filtered_post$lordo.totale, frequency=365)
decomposition_annual <- stl(ts_data_annual, s.window="periodic")

# Convert the decompositions to data frames for ggplot
df_weekly <- as.data.frame(decomposition_weekly$time.series)
df_annual <- as.data.frame(decomposition_annual$time.series)

# Plot using ggplot
p1 <- ggplot(df_weekly, aes(x=seq_along(seasonal))) + 
  geom_line(aes(y=seasonal), color="blue") + 
  labs(title="Weekly Seasonality", y="Value") + theme_minimal()

p2 <- ggplot(df_annual, aes(x=seq_along(seasonal))) + 
  geom_line(aes(y=seasonal), color="red") + 
  labs(title="Annual Seasonality", y="Value") + theme_minimal()

# Display plots side by side
grid.arrange(p1, p2, ncol=2)
print(p1)
print(p2)


## ----------------------------------------------------------------------------------------------------------------------------------


data <- ts(filtered_post$lordo.totale, frequency = 1)

result <- rolling_forecast_function(data, model_type = "auto.arima", h = c(7,14,23,70), origins = 20, boxcox = TRUE)
result <- result[[1]]
forecasts <- result[[2]]
model <- result[[3]]

print(paste("Results: ", result))

 
result


## ----------------------------------------------------------------------------------------------------------------------------------


# Define training and test data
n <- length(filtered_post$lordo.totale)
train <- filtered_post$lordo.totale[1:(0.8*n)]
test <- filtered_post$lordo.totale[(0.8*n + 1):n]

# Fit a SARIMA(7,1,4)(0,1,0)[7] model
model_sarima <- auto.arima(train, seasonal = TRUE)

# Forecast the test data
forecasts <- forecast(model_sarima, h = length(test))
summary(model_sarima)
# Calculate RMSE for the test set
rmse_test <- sqrt(mean((test - forecasts$mean)^2))
print(paste("RMSE on the test set:", rmse_test))

# Plot residuals
residuals <- residuals(model_sarima)
plot(residuals, main="Residuals from SARIMA model with weekly aggregation")
checkresiduals(model_sarima)
# Perform Ljung-Box test
ljung_box_test <- Box.test(residuals, type = "Ljung-Box", lag=10)
print(ljung_box_test)

checkresiduals(model_sarima)



## ----------------------------------------------------------------------------------------------------------------------------------
filtered_post <- filtered_post %>%
  mutate(week = lubridate::isoweek(date)) %>%
  group_by(week) %>%
  summarise(lordo.totale = mean(lordo.totale, na.rm = TRUE))
  
# Log transformation
log_series <- log(filtered_post$lordo.totale)

# 1st order difference transformation
diff_log_series <- diff(log_series)

# Define training and test data
n <- length(diff_log_series)
train <- diff_log_series[1:(0.8*n)]
test <- diff_log_series[(0.8*n + 1):n]

# Fit a SARIMA model on transformed data
model_sarima <- auto.arima(train, seasonal = TRUE)
summary(model_sarima)

# Forecast the test data
forecasts <- forecast(model_sarima, h = length(test))

# Revert transformations for forecasts
last_log_value <- log_series[(0.8*n)]
reverted_diff_forecasts <- diffinv(forecasts$mean, lag=1, differences = 1, xi = last_log_value)
reverted_forecasts <- exp(reverted_diff_forecasts)

# Actual test values (without transformation)
actual_values <- filtered_post$lordo.totale[(0.8*n + 1):n]

# Calculate RMSE for the test set on the original scale
rmse_test <- sqrt(mean((actual_values - reverted_forecasts)^2))
print(paste("RMSE on the test set:", rmse_test))
# Calculate MAPE on the test set
mape_test <- mean(abs((test - forecasts$mean) / test)) * 100
print(paste("MAPE on the test set:", mape_test, "%"))

# Check residuals and perform Ljung-Box test
residuals <- residuals(model_sarima)
plot(residuals, main="Residuals from SARIMA model with weekly aggregation")
checkresiduals(model_sarima)

ljung_box_test <- Box.test(residuals, type = "Ljung-Box", lag=10)
print(ljung_box_test)




## ----------------------------------------------------------------------------------------------------------------------------------
library(forecast)
library(tseries)

# Box-Cox transformation
lambda <- BoxCox.lambda(filtered_post$lordo.totale)
bc_series <- BoxCox(filtered_post$lordo.totale, lambda=lambda)

# 1st order difference transformation
diff_bc_series <- diff(bc_series)

# Define training and test data
n <- length(diff_bc_series)
train <- diff_bc_series[1:(0.8*n)]
test <- diff_bc_series[(0.8*n + 1):n]

# Fit a SARIMA model on transformed data
model_sarima <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(model_sarima)

# Forecast the test data
forecasts <- forecast(model_sarima, h = length(test))

# Revert transformations for forecasts
last_bc_value <- bc_series[(0.8*n)]
reverted_diff_forecasts <- diffinv(forecasts$mean, lag=1, differences = 1, xi = last_bc_value)
reverted_forecasts <- InvBoxCox(reverted_diff_forecasts, lambda)

# Actual test values (without transformation)
actual_values <- filtered_post$lordo.totale[(0.8*n + 1):n]

# Calculate RMSE for the test set on the original scale
rmse_test <- sqrt(mean((actual_values - reverted_forecasts)^2))
print(paste("RMSE on the test set:", rmse_test))

# Calculate MAPE on the test set
mape_test <- mean(abs((actual_values - reverted_forecasts) / actual_values)) * 100
print(paste("MAPE on the test set:", mape_test, "%"))

# Check residuals and perform Ljung-Box test
residuals <- residuals(model_sarima)
plot(residuals, main="Residuals from SARIMA model with Box-Cox transformation")
checkresiduals(model_sarima)

ljung_box_test <- Box.test(residuals, type = "Ljung-Box", lag=10)
print(ljung_box_test)



## ----------------------------------------------------------------------------------------------------------------------------------
library(forecast)
library(tseries)

# Define training and test data without any transformation
n <- length(filtered_post$lordo.totale)
train <- filtered_post$lordo.totale[1:(0.8*n)]
test <- filtered_post$lordo.totale[(0.8*n + 1):n]

# Fit a SARIMA model
model_sarima <- auto.arima(train, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(model_sarima)

# Forecast the test data
forecasts <- forecast(model_sarima, h = length(test))

# Calculate RMSE for the test set
rmse_test <- sqrt(mean((test - forecasts$mean)^2))
print(paste("RMSE on the test set:", rmse_test))

# Calculate MAPE on the test set
mape_test <- mean(abs((test - forecasts$mean) / test)) * 100
print(paste("MAPE on the test set:", mape_test, "%"))

# Check residuals and perform Ljung-Box test
residuals <- residuals(model_sarima)
plot(residuals, main="Residuals from SARIMA model without transformation")
checkresiduals(model_sarima)

ljung_box_test <- Box.test(residuals, type = "Ljung-Box", lag=10)
print(ljung_box_test)



## ----------------------------------------------------------------------------------------------------------------------------------
library(forecast)
library(ggplot2)

# 1. First-differencing the series
diff_series <- diff(filtered_post$lordo.totale, differences = 1)

# 2. Splitting into training and test set (80% training, 20% test)
n <- length(diff_series)
train <- diff_series[1:(0.8*n)]
test <- diff_series[(0.8*n + 1):n]

# 3. Fit an ARIMA model on the differenced data
model_arima <- auto.arima(train, seasonal = TRUE)
summary(model_arima)

# 4. Forecast the test data
forecasts <- forecast(model_arima, h = length(test))

# Revert the differencing to get the forecasts on the original scale
last_original_value <- filtered_post$lordo.totale[(0.8*n)]
reverted_forecasts <- diffinv(forecasts$mean, differences = 1, xi = last_original_value)

# Actual test values
actual_values <- filtered_post$lordo.totale[(0.8*n + 2):n + 1]

# Plot the forecasts against the actual values
#autoplot(forecast(model_arima, h=length(test))) + autolayer(actual_values, series="Actual") + xlab("Time") + ylab("Value")

# 5. Calculate RMSE for the test set on the original scale
rmse_test <- sqrt(mean((actual_values - reverted_forecasts)^2))
print(paste("RMSE on the test set:", rmse_test))

# Calculate MAPE on the test set
mape_test <- mean(abs((actual_values - reverted_forecasts) / actual_values)) * 100
print(paste("MAPE on the test set:", mape_test, "%"))



## ----------------------------------------------------------------------------------------------------------------------------------
# Generate Fourier terms for the entire dataset
fourier_terms <- fourier(ts(filtered_post$lordo.totale, frequency=52), K=3)

# Splitting the data and Fourier terms
train_size <- floor(0.8 * nrow(filtered_post))
train <- filtered_post[1:train_size, ]
test <- filtered_post[(train_size + 1):nrow(filtered_post), ]

train_fourier <- fourier_terms[1:train_size, ]
test_fourier <- fourier_terms[(train_size + 1):nrow(fourier_terms), ]

# Convert the training data to a time series object
train_ts <- ts(train$lordo.totale, frequency=52)
set.seed(2)
# Fit ARIMA model with Fourier terms
model_fourirer <- auto.arima(train_ts, xreg=train_fourier, lambda="auto", stepwise=TRUE, seasonal = TRUE, approximation=FALSE)

# In-sample forecast to get predictions on the training set
in_sample_forecasts <- fitted(model_fourirer)
rmse_train <- sqrt(mean((train$lordo.totale - in_sample_forecasts)^2))
print(paste("RMSE on the training set:", rmse_train))
summary(model_fourirer)
# Forecast on the test set
forecasts <- forecast(model_fourirer, xreg=test_fourier, h=nrow(test))
rmse_test <- sqrt(mean((test$lordo.totale - forecasts$mean)^2))
print(paste("RMSE on the test set:", rmse_test))
accuracy(forecasts$mean, test$lordo.totale)
checkresiduals(model_fourirer)
plot(resid(model_fourirer))


## ----------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(forecast)

 #Filter, add the dummy variable for 'Colore', and aggregate by ISO week
filtered_post <- df %>%
  filter(date >= start_date & date <= end_date) %>%
  mutate(rosso_dummy = ifelse(Colore == "Rossa", 1, 0)) %>%
  mutate(week = lubridate::isoweek(date)) %>%
  group_by(week) %>%
  summarise(lordo.totale = mean(lordo.totale, na.rm = TRUE),
            rosso_dummy = sum(rosso_dummy)) %>%
  mutate(rosso_dummy = ifelse(rosso_dummy > 0, 1, 0))

n <- nrow(filtered_post)

 #Partition data into training and test sets
train_size <- round(0.8 * n)
train <- filtered_post[1:train_size, ]
test <- filtered_post[(train_size + 1):n, ]

#Create a time series object for the training set and the corresponding dummy
train_ts <- ts(train$lordo.totale, frequency = 52)
dummy_train <- train$rosso_dummy
#Fit the ARIMA model with the 'Rossa' dummy variable
model_sarima <- auto.arima(train_ts, seasonal = TRUE, xreg = dummy_train)

#Forecast on the test set using the model
forecasts <- forecast(model_sarima, h = nrow(test), xreg = test$rosso_dummy)

#Output model summary and calculate RMSE
summary(model_sarima)
rmse_test <- sqrt(mean((test$lordo.totale - forecasts$mean)^2))
print(paste("RMSE on the test set:", rmse_test))

 #Plot residuals
residuals <- residuals(model_sarima)
plot(residuals, main = "Residuals from SARIMA model with Rossa Dummy")
checkresiduals(model_sarima)
ljung_box_test <- Box.test(residuals, type = "Ljung-Box", lag = 10)
print(ljung_box_test)




## ----------------------------------------------------------------------------------------------------------------------------------
tbats_model <- tbats(train_vendite)
forecast_tbats <- forecast(tbats_model, h=length(test_vendite))
rmse_tbats <- sqrt(mean((test_vendite - forecast_tbats$mean)^2))
mape_tbats <- mean(abs((test_vendite - forecast_tbats$mean) / test_vendite)) * 100
print(paste("RMSE for TBATS:", rmse_tbats))
print(paste("MAPE for TBATS:", mape_tbats, "%"))
print(tbats_model)


## ----------------------------------------------------------------------------------------------------------------------------------
filtered_df_res$lag_1 <- lag(filtered_df_res$lordo.totale, 1)
filtered_df_res$lag_7 <- lag(filtered_df_res$lordo.totale, 7)

filtered_df$lag_1 <- lag(filtered_df$lordo.totale, 1)
filtered_df$lag_7 <- lag(filtered_df$lordo.totale, 7)
filtered_df
filtered_df_res <- filtered_df_res[-(1:7), ]
filtered_df_res <- na.omit(filtered_df_res)

filtered_df <- filtered_df[-(1:7), ]
filtered_df <- na.omit(filtered_df)




## ----------------------------------------------------------------------------------------------------------------------------------

library("MLmetrics")
library(Metrics) 
r = c('R000', 'R001', 'R002', 'R003','R004', 'R005')

for (re in r) {
  d <- filtered_post %>%
    filter(ristorante == re & lordo.totale != 0)

    train_size <- floor(0.8 * nrow(d))
    train <- d[1:train_size, ]
    test <- d[(train_size + 1):nrow(d), ]
    # Convert the training data to a time series object with a frequency of 7 (weekly seasonality)
    train_ts <- ts(train$lordo.totale, frequency=7)
    prophet_df <- data.frame(ds = train$date, y = train$lordo.totale)
    set.seed(13)
    model_prophet <- prophet(prophet_df)
    future <- make_future_dataframe(model_prophet, periods = nrow(test))
    forecasts_prophet <- predict(model_prophet, future)
    
    
    accuracy(forecasts_prophet$yhat[1:nrow(train)], train$lordo.totale)
    accuracy(forecasts_prophet$yhat[(nrow(train)+1):nrow(forecasts_prophet)], test$lordo.totale)
    train_rmse <- sqrt(mean((train$lordo.totale - forecasts_prophet$yhat[1:nrow(train)])^2))
    test_rmse <- sqrt(mean((test$lordo.totale - forecasts_prophet$yhat[(nrow(train)+1):nrow(forecasts_prophet)])^2))
    test_mape<- mean(abs((test$lordo.totale - forecasts_prophet$yhat[(nrow(train)+1):nrow(forecasts_prophet)]) / test$lordo.totale), na.rm = TRUE) * 100
    print(paste("RMSE on the test set Func:", test_rmse))
    print(paste("MAPE on the test set Func:", test_mape))
    print(paste("RMSE on the test set:", rmse(test$lordo.totale, forecasts_prophet$yhat[(nrow(train)+1):nrow(forecasts_prophet)])))
    print(paste("MAPE on the test set:", MAPE(test$lordo.totale, forecasts_prophet$yhat[(nrow(train)+1):nrow(forecasts_prophet)])))
    accuracy(forecasts_prophet$yhat[(nrow(train)+1):nrow(forecasts_prophet)], test$lordo.totale)


  print(paste("Mean in predicted period:", mean(forecasts_prophet$yhat[1:nrow(train)])))
  print(paste("Total in predicted period:", mean(forecasts_prophet$yhat[1:nrow(train)]) *70))

}


## ----------------------------------------------------------------------------------------------------------------------------------

library("MLmetrics")
library(Metrics) 
r <- c("R000","R001", "R002", "R003", "R004", "R005")

for (re in r) {
  d <- filtered_post %>%
    filter(ristorante == re & lordo.totale != 0)

    train_size <- floor(0.8 * nrow(d))
    train <- d[1:train_size, ]
    test <- d[(train_size + 1):nrow(d), ]
    # Convert the training data to a time series object with a frequency of 7 (weekly seasonality)
    train_ts <- ts(train$lordo.totale, frequency=7)
model_ets <- ets(train_ts, model="ZZZ")#, damped=NULL, lambda=TRUE,allow.multiplicative.trend=TRUE,biasadj=TRUE)
print("ETS Model Summary:")
summary(model_ets)
forecasts_ets <- forecast(model_ets, h=nrow(test))
plot(forecasts_ets)
print("Test Set Accuracy Metrics for ETS:")
accuracy(forecasts_ets$mean, test$lordo.totale)

# Calculate RMSE for the test set using ETS
rmse_value_ets <- sqrt(mean((test$lordo.totale - forecasts_ets$mean)^2))
print(paste("RMSE on the test set using ETS:", rmse_value_ets))
mape_test_ets <- mean(abs((test$lordo.totale - forecasts_ets$mean) / test$lordo.totale)) * 100

print(paste("MAPE on the test set using ETS:", mape_test_ets))


  print(paste("Mean in predicted period:", mean(forecasts_ets$mean)))
  print(paste("Total in predicted period:", mean(forecasts_ets$mean)*70))

}


## ----------------------------------------------------------------------------------------------------------------------------------

library("MLmetrics")
library(Metrics) 

for (re in r) {
  d <- filtered_post %>%
    filter(ristorante == re & lordo.totale != 0)

    train_size <- floor(0.8 * nrow(d))
    train <- d[1:train_size, ]
    test <- d[(train_size + 1):nrow(d), ]
    # Convert the training data to a time series object with a frequency of 7 (weekly seasonality)
train_ts <- ts(train$lordo.totale, frequency=7)
set.seed(1)
model_tbats <- tbats(train_ts)
print("TBATS Model Summary:")
summary(model_tbats)
forecasts_tbats <- forecast(model_tbats, h=nrow(test))
# Calculate RMSE for the test set using ETS
rmse_value_ets <- sqrt(mean((test$lordo.totale - forecasts_tbats$mean)^2))
print(paste("RMSE on the test set using ETS:", rmse_value_ets))
mape_test_ets <- mean(abs((test$lordo.totale - forecasts_tbats$mean) / test$lordo.totale)) * 100

print(paste("MAPE on the test set using ETS:", mape_test_ets))


print(paste("Mean in predicted period:", mean(forecasts_tbats$mean)))
print(paste("Total in predicted period:", mean(forecasts_tbats$mean)*70))

}


## ----------------------------------------------------------------------------------------------------------------------------------
r = c('R000', 'R001', 'R002', 'R003','R004', 'R005')

for (re in r) {

filtered_post <- filtered_post %>%
  mutate(
    lordo.totale_lag1 = lag(lordo.totale, 1),
    lordo.totale_lag2 = lag(lordo.totale, 2),
    lordo.totale_lag7 = lag(lordo.totale, 7)
  )
  d <- filtered_post %>%
    filter(ristorante == re & lordo.totale != 0)

# Encode Giorno as weekend or not, assuming 6 and 7 represent weekend days.
# filtered_df$Weekend <- ifelse(filtered_df$Giorno %in% c(6, 7), 1, 0)



# Split the data
    train_size <- floor(0.8 * nrow(d))
    train <- d[1:train_size, ]
    test <- d[(train_size + 1):nrow(d), ]
set.seed(1)

# Fit a Bayesian linear regression model with Giorno, multiple lags and possibly weekend
model_bayesian_with_lags <- stan_glm(lordo.totale ~ Giorno +Festivo_New+Festivo_or_Weekend_New+ Weekend_New+Season+ lordo.totale_lag1 + lordo.totale_lag2 + lordo.totale_lag7 + Colore + Lockdown , 
                                     data = train)


# Predict on the test set
predictions <- predict(model_bayesian_with_lags, newdata = test)

# Compute RMSE
RMSE <- sqrt(mean((predictions - test$lordo.totale)^2))

# Calculate RMSE for the test set using ETS
rmse_value_ets <- sqrt(mean((predictions - test$lordo.totale)^2))
print(paste("RMSE on the test set using ETS:", rmse_value_ets))
mape_test_ets <- mean(abs((test$lordo.totale - predictions) / test$lordo.totale)) * 100

print(paste("MAPE on the test set using ETS:", mape_test_ets))


  print(paste("Mean in predicted period:", mean(predictions)))
  print(paste("Total in predicted period:", mean(predictions)*70))

}

predictions


## ----------------------------------------------------------------------------------------------------------------------------------
g = forecasts_prophet$yhat[nrow(train) : length(forecasts_prophet$yhat)]
g[1]
g = g[2:length(g)]
g  
MAPE(g, test$lordo.totale)

rmse(test$lordo.totale, g)

 
mean(abs((test$lordo.totale - forecasts_prophet$yhat[(nrow(train)+1):nrow(forecasts_prophet)]) / test$lordo.totale), na.rm = TRUE) * 100



## ----------------------------------------------------------------------------------------------------------------------------------
library("MLmetrics")
library(Metrics) 

for (re in r) {
  d <- filtered_post %>%
    filter(ristorante == re & lordo.totale != 0)

    train_size <- floor(0.8 * nrow(d))
    train <- d[1:train_size, ]
    test <- d[(train_size + 1):nrow(d), ]
    # Convert the training data to a time series object with a frequency of 7 (weekly seasonality)
train_ts <- ts(train$lordo.totale, frequency=7)
set.seed(1)








vendite_ts <- ts(d$lordo.totale,  frequency = (365.25/7))
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
hw_model <- HoltWinters(train_vendite,seasonal = "multiplicative" ,beta = FALSE, gamma = FALSE)
forecast_hw <- forecast(hw_model, h=length(test_vendite))






model_tbats <- tbats(train_ts)
print("TBATS Model Summary:")
summary(model_tbats)
# Calculate RMSE for the test set using ETS
rmse_value_ets <- sqrt(mean((test$lordo.totale - forecast_hw$mean)^2))
print(paste("RMSE on the test set using ETS:", rmse_value_ets))
mape_test_ets <- mean(abs((test$lordo.totale - forecast_hw$mean) / test$lordo.totale)) * 100

print(paste("MAPE on the test set using ETS:", mape_test_ets))



}


## ----------------------------------------------------------------------------------------------------------------------------------
library(randomForest)
library(dplyr)
filtered_post <- filtered_post %>%
  mutate(
    lordo.totale_lag1 = lag(lordo.totale, 1),
    lordo.totale_lag2 = lag(lordo.totale, 2),
    lordo.totale_lag7 = lag(lordo.totale, 7)
  )
filtered_post <- filtered_post[-(1:7), ]
filtered_post <- na.omit(filtered_post)
# Adjust the size of the plotting device
options(repr.plot.width = 8, repr.plot.height = 6)

r <- c("R000", "R001", "R002", "R003", "R004", "R005")

# Create a layout with 2 columns and 3 rows
par(mfrow = c(3, 2))

# Set smaller margins
par(mar = c(2.5, 2.5, 2.5, 2.5))

for (re in r) {
  d <- filtered_post %>%
    filter(ristorante == re & lordo.totale != 0)

  train_size <- floor(0.8 * nrow(d))
  train_set <- d[1:train_size, ]
  test_set <- d[(train_size + 1):nrow(d), ]
  print(train_set)
  train_set
  model <- randomForest(lordo.totale ~ Giorno + Weekend_New + Festivo_or_Weekend_New + lordo.totale_lag7 + lordo.totale_lag2 + lordo.totale_lag1 + Colore + Lockdown, data = train_set)
  #print(paste("Feature Importance ", re, ": "))
  # Assuming your variable importance plot data frame is named 'var_imp_df'
  #rownames(model$importance) <- c("Day", "Weekend", "Holiday or Weekend", "Lag 7", "Lag 2", "Lag 1", "Color", "Lockdown")
  #print(model$importance)

  # Now create the variable importance plot
  #varImpPlot(model, main = paste(re), pch = 19, col = "orange", cex = 1.5)
predictions <- predict(model, newdata=test_set)
importance(rf_model)
varImpPlot(rf_model)
rmse <- sqrt(mean((test_set$lordo.totale - predictions)^2))

print(paste("RMSE on the test set :", rmse))
mape_test_ets <- mean(abs((test_set$lordo.totale - predictions) / test_set$lordo.totale)) * 100

print(paste("MAPE on the test set:", mape_test_ets))

}

# Reset the layout and margins to the default
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)



## ----------------------------------------------------------------------------------------------------------------------------------


for (re in r) {
  d <- filtered_post %>%
    filter(ristorante == re & lordo.totale != 0)

d$lag_1 <- lag(d$lordo.totale, 1)
d$lag_2 <- lag(d$lordo.totale, 2)
d$lag_7 <- lag(d$lordo.totale, 7)
d <- d[8:nrow(d), ]
colore_dummies <- model.matrix(~ Colore, data = d)[,-1]  # Drop first column
giorno_dummies <- model.matrix(~ Giorno, data = d)[,-1] 
combined_xreg <- cbind(colore_dummies, giorno_dummies, d$lag_1, d$lag_2, d$lag_7)
vendite_ts <- ts(d$lordo.totale, frequency = 1)
train_size <- floor(0.8 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
train_predictors <- combined_xreg[1:train_size, ]
test_predictors <- combined_xreg[(train_size + 1):nrow(combined_xreg), ]
fit <- auto.arima(train_vendite, xreg=train_predictors, seasonal=TRUE)
forecasted <- forecast(fit, xreg=test_predictors, h=length(test_vendite))
rmse <- sqrt(mean((test_vendite - forecasted$mean)^2))
print(paste("RMSE for SARIMAX with 'Colore', 'Giorno', and Lagged Variables:", rmse))
mape_test_ets <- mean(abs((test_vendite - forecasted$mean) / test_vendite)) * 100

print(paste("MAPE on the test set:", mape_test_ets))

}

## ----------------------------------------------------------------------------------------------------------------------------------


for (re in r) {
  d <- filtered_post %>%
    filter(ristorante == re & lordo.totale != 0)

  
  
vendite_ts <- ts(d$lordo.totale, frequency = 365.25/7)
K <- 3
fourier_terms <- fourier(vendite_ts, K=K)
colore_dummies <- model.matrix(~ Colore - 1, data = d)[,-1]  # drop the first column
giorno_dummies <- model.matrix(~ Giorno - 1, data = d)[,-1]  # drop the first column
combined_xreg <- cbind(fourier_terms, colore_dummies, giorno_dummies)
train_size <- floor(0.94 * length(vendite_ts))
train_vendite <- vendite_ts[1:train_size]
test_vendite <- vendite_ts[(train_size + 1):length(vendite_ts)]
train_regressors <- combined_xreg[1:train_size,]
test_regressors <- combined_xreg[(train_size + 1):nrow(combined_xreg),]
fit <- auto.arima(train_vendite, xreg=train_regressors, seasonal=TRUE)
forecasted <- forecast(fit, xreg=test_regressors, h=length(test_vendite))
rmse <- sqrt(mean((test_vendite - forecasted$mean)^2))
print(paste("RMSE for ARIMA with Fourier terms, 'Colore', and 'Giorno' dummies:", rmse))
mape_test_ets <- mean(abs((test_vendite - forecasted$mean) / test_vendite)) * 100

print(paste("MAPE on the test set:", mape_test_ets))
}

