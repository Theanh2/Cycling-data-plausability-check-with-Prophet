
#----------------------------#
#Prophet: test / train split each station ----
#----------------------------#
# df_d1 <- df %>% select(-direction_2) %>% rename(y = "direction_1")
# 
# df_Arnulf_d1 <- df_d1 %>% filter(station == "Arnulf")
# df_Erhardt_d1 <- df_d1 %>% filter(station == "Erhardt")
# df_Kreuther_d1 <- df_d1 %>% filter(station == "Kreuther")
# df_Olympia_d1 <- df_d1 %>% filter(station == "Olympia")
# df_Margareten_d1 <- df_d1 %>% filter(station == "Margareten")
# df_Hirsch_d1 <- df_d1 %>% filter(station == "Hirsch")
# 
# df_d2 <- df %>% select(-direction_1)  %>% rename(y = "direction_2")
# 
# df_Arnulf_d2 <- df_d2 %>% filter(station == "Arnulf")
# df_Erhardt_d2 <- df_d2 %>% filter(station == "Erhardt")
# df_Kreuther_d2 <- df_d2 %>% filter(station == "Kreuther")
# df_Olympia_d2 <- df_d2 %>% filter(station == "Olympia")
# df_Margareten_d2 <- df_d2 %>% filter(station == "Margareten")
# df_Hirsch_d2 <- df_d2 %>% filter(station == "Hirsch")

#Fit prophet model across stations
df_train <- df %>% filter(year < 2017)
df_test <- df %>% filter(year > 2016)
df_train <- df_train %>% select(-direction_2)  %>% rename(y = "direction_1")
df_test <- df_test %>% select(-direction_2)  %>% rename(y = "direction_1")

df_train <- merge(df_train, temp_precip, by = c("year", "month", "hour", "day"))
df_train <- df_train %>% drop_na(y, air_temp, precipitation)
df_train_arnulf <- df_train %>% filter(station == "Arnulf")

prophet <- prophet(#mcmc_samples=300,
  changepoint_prior_scale=0.01,
  seasonality_mode='multiplicative',  
  holidays_prior_scale=0.25, 
  holidays = holiday
)

prophet <- add_regressor(prophet, "air_temp", mode='additive')
prophet <- add_regressor(prophet, "precipitation", mode='additive')
prophet <- fit.prophet(prophet, df_train_arnulf)

future = make_future_dataframe(prophet, periods= 1000, freq= 60*15)

future <- extract_yearmonthdayhour(future)
future_weather <- merge(future, temp_precip, by = c("year", "month","day", "hour"))
future_weather <- future_weather %>% drop_na(air_temp, precipitation)
forecast <-  predict(prophet, future_weather)

plot(prophet, forecast)
prophet_plot_components(prophet, forecast)
#to do: add weekend/weekday, 
#prior scale for smoothing

#https://www.artefact.com/blog/is-facebook-prophet-suited-for-doing-good-predictions-in-a-real-world-project/
#https://structural-time-series.fastforwardlabs.com
#https://github.com/fastforwardlabs/structural-time-series
#----------------------------#