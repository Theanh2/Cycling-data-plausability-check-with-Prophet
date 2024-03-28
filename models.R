#Lags----
df_train_Arnulf_d1 %>%
  mutate(lag1 = lag(y)) %>%
  slice_tail(n = -1) -> df_train_Arnulf_d1

df_train_Arnulf_d1_hourly %>%
  mutate(lag1 = lag(direction_1)) %>%
  slice_tail(n = -1) -> df_train_Arnulf_d1_hourly

df_temp_precip_hourly %>%
  mutate(lag1 = lag(direction_1)) %>%
  slice_tail(n = -1) -> df_temp_precip_hourly

df_temp_precip %>%
  mutate(lag1 = lag(direction_1)) %>%
  slice_tail(n = -1) -> df_temp_precip

df_Arnulf_d1_hourly<- df_temp_precip_hourly %>% filter(station == "Arnulf")
df_Arnulf_d1 <- df_temp_precip %>% filter(station == "Arnulf")

df_Arnulf_d1_hourly %>%
  mutate(lag1 = lag(direction_1)) %>%
  slice_tail(n = -1) -> df_Arnulf_d1_hourly

#
#coverage----
#
coverage <- function(mod, df, pred) { 
  coverage <- mean(df$direction_1 <= pred[,2] & df$direction_1 >= pred[,1], na.rm = T)
  cat("Coverage of PI ", deparse(substitute(mod)), ":")
  return(coverage)
}

#
#prediction interval -----
#
pred_dist <- function(train_fit,
                      test_dat,
                      nb_draws){
  ## get the mean and covariance matrix for predictions on the testing data
  test_preds <- predict(train_fit, test_dat, type = "lpmatrix")
  ## replicate parameter vectors
  rep_params <- rmvn(nb_draws,coef(train_fit),train_fit$Vp)
  pred_count <- matrix(0, nrow=nrow(test_dat), ncol=nb_draws)
  ## replicate predictions for each set of parameters
  for (i in 1:nb_draws) {
    rep_preds <- test_preds %*% rep_params[i,]
    pred_count[,i] <- exp(rep_preds)
  }
  ## find the dispersion parameter for the negative binomial
  r <- train_fit$family$getTheta(trans=TRUE)
  ## sample from the negative binomial distribution
  all_preds <- matrix(rnbinom(nrow(pred_count)*nb_draws,
                              mu=as.vector(pred_count), size=r),
                      nrow=nrow(test_dat)) %>%
    as.data.frame()
  all_preds <- apply(all_preds, 1, function(x) quantile(x,c(0.005,0.995), na.rm=T)) %>% t()
  return(all_preds)
}
#
#get outliers----
#
outliers  <- function(mod, df, pred){
           outliers <- c()
           for(i in 1:nrow(df)){
             if(!is.na(df$direction_1[i]) & !is.na(pred[[i,1]]) & !is.na(pred[[i,2]])){
              if(df$direction_1[i] > pred[[i,2]] | df$direction_1[i] < pred[[i,1]] ) {
               outliers <- append(outliers, i)
              }
             }
           }
           print(outliers)
           df_outliers <- df[outliers,]
           return(df_outliers)
}
#
#nb_gam_hour_no_lag-------------------
#
nb_gam_hour_no_lag <-gam(direction_1 ~ 
                        s(air_temp, bs = "bs", k = 20) +
                        s(hour_weekday, bs = "cc", k = 40) +
                        s(month_year, bs = "bs", k = 20) +
                          ind + holiday,
                      family = nb(),
                      data = df_Arnulf_d1_hourly,
                      method = "REML")

plot(nb_gam_hour_no_lag, pages = 1, scale = 0)
summary(nb_gam_hour_no_lag)

nb_gam_hour_no_lag_PI <- pred_dist(nb_gam_hour_no_lag, df_Arnulf_d1_hourly, 1000)
coverage(nb_gam_hour_no_lag, df_Arnulf_d1_hourly, nb_gam_hour_no_lag_PI)
outliers(nb_gam_hour_no_lag, df_Arnulf_d1_hourly,  nb_gam_hour_no_lag_PI)

 df_Arnulf_d1_hourly %>% 
  mutate(lower = nb_gam_hour_no_lag_PI[,1]) %>%
  mutate(upper = nb_gam_hour_no_lag_PI[,2]) %>%
  ggplot(aes(x = 1:nrow(df_Arnulf_d1_hourly))) +
  geom_point(aes(y = direction_1),color = 'red', alpha = 0.25) +
  geom_ribbon(mapping = aes(ymin = lower, ymax= upper), fill = "grey") 
#
#nb_gam_hour_lag-------------------
#
nb_gam_hour_lag <-gam(direction_1 ~ 
                        s(air_temp, bs = "bs", k = 20) +
                        s(hour_weekday, bs = "cc", k = 40) +
                        s(month_year, bs = "bs", k = 20) +
                        s(lag1, bs = "bs", k = 20) +
                       ind + holiday,
                   family = nb(),
                   data = df_Arnulf_d1_hourly,
                   method = "REML")

plot(nb_gam_hour_lag, pages = 1, scale = 0)
summary(nb_gam_hour_lag)

nb_gam_hour_lag_PI <- pred_dist(nb_gam_hour_lag, df_Arnulf_d1_hourly, 1000)
coverage(nb_gam_hour_lag, df_Arnulf_d1_hourly, nb_gam_hour_lag_PI)

df_Arnulf_d1_hourly %>% 
  mutate(lower = nb_gam_hour_lag_PI[,1]) %>%
  mutate(upper = nb_gam_hour_lag_PI[,2]) %>%
  ggplot(aes(x = 1:nrow(df_Arnulf_d1_hourly))) +
  geom_point(aes(y = direction_1,color = 'prediction'), alpha = 0.25) +
  geom_ribbon(mapping = aes(ymin = lower, ymax= upper), fill = "grey") 

#
#nb_gam_min_no_lag-----
#
nb_gam_min_no_lag <-gam(direction_1 ~ 
                          s(air_temp, bs = "bs", k = 20) +
                          s(hour_weekday, bs = "cc", k = 40) +
                          s(month_year, bs = "bs", k = 20) +
                          ind + holiday,
                        family = nb(),
                        data = df_Arnulf_d1)

plot(nb_gam_min_no_lag, pages = 1, scale = 0)
summary(nb_gam_min_no_lag)

nb_gam_min_no_lag_PI <- pred_dist(nb_gam_min_no_lag, df_Arnulf_d1, 1000)
coverage(nb_gam_min_no_lag, df_Arnulf_d1, nb_gam_min_no_lag_PI)

df_Arnulf_d1 %>%
  mutate(lower = nb_gam_min_no_lag_PI[,1]) %>%
  mutate(upper = nb_gam_min_no_lag_PI[,2]) %>%
  ggplot(aes(x = 1:nrow(df_Arnulf_d1))) +
  geom_point(aes(y = direction_1,color = 'prediction'), alpha = 0.25) +
  geom_ribbon(mapping = aes(ymin = lower, ymax= upper), fill = "grey")
#
#nb_gam_min_lag-------------------
#
nb_gam_min_lag <-gam(direction_1 ~ 
                       s(air_temp, bs = "bs", k = 20) +
                       s(hour_weekday, bs = "cc", k = 40) +
                       s(month_year, bs = "bs", k = 20) +
                       s(lag1, bs = "bs", k = 20) +
                       ind + holiday,
                     family = nb(),
                     data = df_Arnulf_d1)

plot(nb_gam_min_lag, pages = 1, scale = 0)
summary(nb_gam_min_lag)

nb_gam_min_lag_PI <- pred_dist(nb_gam_min_lag, df_Arnulf_d1, 1000)
coverage(nb_gam_min_lag, df_Arnulf_d1, nb_gam_min_no_lag_PI)

df_Arnulf_d1 %>%
  mutate(lower = nb_gam_min_lag_PI[,1]) %>%
  mutate(upper = nb_gam_min_lag_PI[,2]) %>%
  ggplot(aes(x = 1:nrow(df_Arnulf_d1))) +
  geom_point(aes(y = direction_1,color = 'prediction'), alpha = 0.25) +
  geom_ribbon(mapping = aes(ymin = lower, ymax= upper), fill = "grey")

 




