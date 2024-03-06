#--------------------#
#----Load Packages----
#--------------------#
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(mgcv)
library(ggplot2)
library(prophet)
library(prophetuneR)

#------------------------------------------------------------------#
#----Prophet: test / train split each station for each direction----
#------------------------------------------------------------------#
# Note: 
# 1. Train set: year 2008 ~ 2019 (about 80%) / Test set: year 2020 ~ 2022 (about 20%)
# 2. As discussed, let's consider only air_temperature and precipitation for weather information

# direction_1
df_train_d1 <-  
  df_temp_precip %>% 
  rename(y = "direction_1") %>%
  filter(year < 2020) %>%
  drop_na(y, air_temp, precipitation)

df_test_d1 <- 
  df_temp_precip %>% 
  rename(y = "direction_1") %>%
  filter(year >= 2020) %>%
  drop_na(y, air_temp, precipitation)

df_train_Arnulf_d1 <- df_train_d1 %>% filter(station == "Arnulf")
df_train_Erhardt_d1 <- df_train_d1 %>% filter(station == "Erhardt")
df_train_Kreuther_d1 <- df_train_d1 %>% filter(station == "Kreuther")
df_train_Olympia_d1 <- df_train_d1 %>% filter(station == "Olympia")
df_train_Margareten_d1 <- df_train_d1 %>% filter(station == "Margareten")
df_train_Hirsch_d1 <- df_train_d1 %>% filter(station == "Hirsch")

df_test_Arnulf_d1 <- df_test_d1 %>% filter(station == "Arnulf")
df_test_Erhardt_d1 <- df_test_d1 %>% filter(station == "Erhardt")
df_test_Kreuther_d1 <- df_test_d1 %>% filter(station == "Kreuther")
df_test_Olympia_d1 <- df_test_d1 %>% filter(station == "Olympia")
df_test_Margareten_d1 <- df_test_d1 %>% filter(station == "Margareten")
df_test_Hirsch_d1 <- df_test_d1 %>% filter(station == "Hirsch")

# direction_2
df_train_d2 <- 
  df_temp_precip %>% 
  rename(y = "direction_2") %>%
  filter(year < 2020) %>%
  drop_na(y, air_temp, precipitation)

df_test_d2 <- 
  df_temp_precip %>% 
  rename(y = "direction_2") %>%
  filter(year >= 2020) %>%
  drop_na(y, air_temp, precipitation)

df_train_Arnulf_d2 <- df_train_d2 %>% filter(station == "Arnulf")
df_train_Erhardt_d2 <- df_train_d2 %>% filter(station == "Erhardt")
df_train_Kreuther_d2 <- df_train_d2 %>% filter(station == "Kreuther")
df_train_Olympia_d2 <- df_train_d2 %>% filter(station == "Olympia")
df_train_Margareten_d2 <- df_train_d2 %>% filter(station == "Margareten")
df_train_Hirsch_d2 <- df_train_d2 %>% filter(station == "Hirsch")

df_test_Arnulf_d2 <- df_test_d2 %>% filter(station == "Arnulf")
df_test_Erhardt_d2 <- df_test_d2 %>% filter(station == "Erhardt")
df_test_Kreuther_d2 <- df_test_d2 %>% filter(station == "Kreuther")
df_test_Olympia_d2 <- df_test_d2 %>% filter(station == "Olympia")
df_test_Margareten_d2 <- df_test_d2 %>% filter(station == "Margareten")
df_test_Hirsch_d2 <- df_test_d2 %>% filter(station == "Hirsch")


#------------------------------------#
#----Fit prophet model per station----
#------------------------------------#
# Note: At the moment, I do not filter outliers out in advance

# direction_1
# Arnulf
fit_Arnulf_d1 <- prophet(
  growth = "linear",
  holidays = holiday,
  seasonality.mode = "additive",
  seasonality.prior.scale = 10,
  holidays.prior.scale = 5,
  changepoint.prior.scale = 0.5
)
fit_Arnulf_d1 <- add_regressor(fit_Arnulf_d1, "air_temp", mode = "additive")
fit_Arnulf_d1 <- add_regressor(fit_Arnulf_d1, "precipitation", mode = "additive")
Sys.time()
fit_Arnulf_d1 <- fit.prophet(fit_Arnulf_d1, df_train_Arnulf_d1)
Sys.time() # took 3 mins

future_Arnulf_d1 <- make_future_dataframe(fit_Arnulf_d1, periods = 1000, freq = 60*15)
future_Arnulf_d1 <- extract_yearmonthdayhour(future_Arnulf_d1)
future_Arnulf_d1_weather <- merge(future_Arnulf_d1, temp_precip, by = c("year", "month","day", "hour"))
future_Arnulf_d1_weather <- future_Arnulf_d1_weather %>% drop_na(air_temp, precipitation) %>% select(-c(quality_level, humidity))

# 2:06
forecast_Arnulf_d1 <- predict(fit_Arnulf_d1, future_Arnulf_d1_weather)
# 
forecast <-  predict(prophet, future_weather)

plot(prophet, forecast)
prophet_plot_components(prophet, forecast)

#prior scale for smoothing


#----------------------------#
#flat Prophet Model for Erhardt----

df_Erhardt_weather_d1 <- merge(df_Erhardt_d1, temp_precip, by = c("year", "month", "hour", "day"))
df_Erhardt_weather_d1 <- df_Erhardt_weather_d1 %>% filter(year > 2018)

prophet_Erhardt_d1 <- prophet(mcmc_samples=300, 
                              growth = "flat",
    changepoint_prior_scale=0.5,
    seasonality_mode='multiplicative',  
    holidays_prior_scale=0.25, 
    holidays = holiday,
    daily.seasonality = TRUE, #n = 4
    weekly.seasonalit = 7, # n = 3
    yearly.seasonality = TRUE #n = 10
  )
prophet_Erhardt_d1 <- add_regressor(prophet_Erhardt_d1, "air_temp", prior_scale=0.5, mode='multiplicative')
prophet_Erhardt_d1 <- add_regressor(prophet_Erhardt_d1, "precipitation", prior_scale=0.5, mode='multiplicative')

prophet_Erhardt_d1 <- fit.prophet(prophet_Erhardt_d1, df_Erhardt_weather_d1)

#save fitted model
saveRDS(prophet_Erhardt_d1, file="prophet_Erhardt_d1_flat.RDS")
  
future_Erhardt_d1 = make_future_dataframe(prophet_Erhardt_d1, periods= 4*24*30, freq= 60*15) #forecast next month in 15 min intervals
future_Erhardt_d1 <- extract_yearmonthdayhour(future_Erhardt_d1) 
future_weather_Erhardt_d1 <- merge(future_Erhardt_d1, temp_precip, by = c("year", "month","day", "hour")) #merge future dataframe with weather data
future_weather_Erhardt_d1 <- future_weather_Erhardt_d1 %>% drop_na(air_temp, precipitation) #omit NAs

forecast_Erhardt_d1 <-  predict(prophet_Erhardt_d1, future_weather_Erhardt_d1) #predict

#save forecast
saveRDS(forecast_Erhardt_d1, file="forecast_Erhardt_d1_flat.rds")

plot(prophet_Erhardt_d1, forecast_Erhardt_d1)
prophet_plot_components(prophet_Erhardt_d1, forecast_Erhardt_d1)
  
#Diagnostics 
#df_cv_Erhardt_d1 <- cross_validation(model = prophet_Erhardt_d1, initial = 365*2, period = 180, horizon = 30, units = 'days')
#df_p_Erhardt_d1 <- performance_metrics(df_cv_Erhardt_d1)

#clipping flat model ----
forecast_Erhardt_d1_clipped <- forecast_Erhardt_d1
forecast_Erhardt_d1_clipped$yhat[forecast_Erhardt_d1_clipped$yhat<0] <- 0
forecast_Erhardt_d1_clipped$yhat_lower[forecast_Erhardt_d1_clipped$yhat_lower<0] <- 0
forecast_Erhardt_d1_clipped$yhat_upper[forecast_Erhardt_d1_clipped$yhat_upper<0] <- 0

plot(prophet_Erhardt_d1, forecast_Erhardt_d1_clipped)

prophet_plot_components(prophet_Erhardt_d1, forecast_Erhardt_d1_clipped)
#logistic growth Prophet Model for Erhardt----

df_Erhardt_weather_d1$floor <- 0
df_Erhardt_weather_d1$cap <- 2000
prophet_Erhardt_d1_log <- prophet(mcmc_samples=300, 
                              growth = "logistic",
                              changepoint_prior_scale=0.5,
                              seasonality_mode='multiplicative',  
                              holidays_prior_scale=0.25, 
                              holidays = holiday,
                              daily.seasonality = TRUE, #n = 4
                              weekly.seasonalit = 7, # n = 3
                              yearly.seasonality = TRUE #n = 10
)
prophet_Erhardt_d1_log <- add_regressor(prophet_Erhardt_d1_log, "air_temp", prior_scale=0.5, mode='multiplicative')
prophet_Erhardt_d1_log <- add_regressor(prophet_Erhardt_d1_log, "precipitation", prior_scale=0.5, mode='multiplicative')

prophet_Erhardt_d1_log <- fit.prophet(prophet_Erhardt_d1_log, df_Erhardt_weather_d1)

#save fitted model
saveRDS(prophet_Erhardt_d1, file="prophet_Erhardt_d1_log.RDS")

future_Erhardt_d1_log = make_future_dataframe(prophet_Erhardt_d1_log, periods= 4*24*30, freq= 60*15) #forecast next month in 15 min intervals
future_Erhardt_d1_log <- extract_yearmonthdayhour(future_Erhardt_d1_log) 
future_weather_Erhardt_d1_log <- merge(future_Erhardt_d1_log, temp_precip, by = c("year", "month","day", "hour")) #merge future dataframe with weather data
future_weather_Erhardt_d1_log <- future_weather_Erhardt_d1_log %>% drop_na(air_temp, precipitation) #omit NAs
future_weather_Erhardt_d1_log$floor <- 0
future_weather_Erhardt_d1_log$cap <- 2000

forecast_Erhardt_d1_log <-  predict(prophet_Erhardt_d1_log, future_weather_Erhardt_d1_log) #predict

#save forecast
saveRDS(forecast_Erhardt_d1_log, file="forecast_Erhardt_d1_log.rds")

plot(prophet_Erhardt_d1_log, forecast_Erhardt_d1_log)
prophet_plot_components(prophet_Erhardt_d1_log, forecast_Erhardt_d1_log)

#Diagnostics 
#df_cv_Erhardt_d1 <- cross_validation(model = prophet_Erhardt_d1, initial = 365*2, period = 180, horizon = 30, units = 'days')
#df_p_Erhardt_d1 <- performance_metrics(df_cv_Erhardt_d1)

#log transform Kreuther ----
df_Erhardt_weather_d1_trans <- df_Erhardt_weather_d1
df_Erhardt_weather_d1_trans$y = log(1+df_Erhardt_weather_d1_trans$y)

prophet_Erhardt_d1_trans <- prophet(mcmc_samples=300, 
                                  changepoint_prior_scale=0.5,
                                  seasonality_mode='multiplicative',  
                                  holidays_prior_scale=0.25, 
                                  holidays = holiday,
                                  daily.seasonality = TRUE, #n = 4
                                  weekly.seasonalit = 7, # n = 3
                                  yearly.seasonality = TRUE #n = 10
)
prophet_Erhardt_d1_trans <- add_regressor(prophet_Erhardt_d1_trans, "air_temp", prior_scale=0.5, mode='multiplicative')
prophet_Erhardt_d1_trans <- add_regressor(prophet_Erhardt_d1_trans, "precipitation", prior_scale=0.5, mode='multiplicative')

prophet_Erhardt_d1_trans <- fit.prophet(prophet_Erhardt_d1_trans, df_Erhardt_weather_d1_trans)

#save fitted model
saveRDS(prophet_Erhardt_d1_trans, file="prophet_Erhardt_d1_trans.RDS")

future_Erhardt_d1_trans = make_future_dataframe(prophet_Erhardt_d1_trans, periods= 4*24*30, freq= 60*15) #forecast next month in 15 min intervals
future_Erhardt_d1_trans <- extract_yearmonthdayhour(future_Erhardt_d1_trans) 
future_weather_Erhardt_d1_trans <- merge(future_Erhardt_d1_trans, temp_precip, by = c("year", "month","day", "hour")) #merge future dataframe with weather data
future_weather_Erhardt_d1_trans <- future_weather_Erhardt_d1_trans %>% drop_na(air_temp, precipitation) #omit NAs

forecast_Erhardt_d1_trans <-  predict(prophet_Erhardt_d1_trans, future_weather_Erhardt_d1_trans) #predict

#save forecast
saveRDS(forecast_Erhardt_d1_trans, file="forecast_Erhardt_d1_trans.rds")

#retransform

forecast_Erhardt_d1_trans[c('yhat','yhat_upper', "yhat_lower")] <- 
  exp(forecast_Erhardt_d1_trans[c('yhat','yhat_upper', "yhat_lower")])

plot(prophet_Erhardt_d1_trans, forecast_Erhardt_d1_trans)
prophet_plot_components(prophet_Erhardt_d1_trans, forecast_Erhardt_d1_trans)



#posProphet changes ----
# trace(prophet:::piecewise_linear, edit=TRUE)
# 
# function (t, deltas, k, m, changepoint.ts) 
# {
#   gammas <- -changepoint.ts * deltas
#   k_t <- rep(k, length(t))
#   m_t <- rep(m, length(t))
#   for (s in 1:length(changepoint.ts)) {
#     indx <- t >= changepoint.ts[s]
#     k_t[indx] <- k_t[indx] + deltas[s]
#     m_t[indx] <- m_t[indx] + gammas[s]
#   }
#   y <- k_t * t + m_t
#   if(max(t) <= 1){
#     return(y)
#   }
# 
# #  indx_future = which.max(t >= 1)
#   while(min(y[indx_future:]) < 0){
#     indx_neg = indx_future + which.max(y[indx_future:] < 0)
#   k_t[indx_neg:] -= k_t[indx_neg]
#   m_t[indx_neg:] -= m_t[indx_neg]
#   y = k_t * t + m_t
#   }
#   
#   return(y)
#}


