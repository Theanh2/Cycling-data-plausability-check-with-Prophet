#Lags----
df_train_Arnulf_d1 %>%
  mutate(lag1 = lag(y)) %>%
  slice_tail(n = -1) -> df_train_Arnulf_d1

df_train_Arnulf_d1_hourly %>%
  mutate(lag1 = lag(y)) %>%
  slice_tail(n = -1) -> df_train_Arnulf_d1_hourly

df_Arnulf_d1_hourly<- df_temp_precip_hourly %>% filter(station == "Arnulf")

  df_Arnulf_d1_hourly %>%
  mutate(lag1 = lag(direction_1)) %>%
  slice_tail(n = -1) -> df_Arnulf_d1_hourly

sum(df_train_Arnulf_d1$precipitation > 0, na.rm = T)
#
#coverage----
#
coverage <- function(mod, df, pred) { 
  ypred = predict(mod, df, type = "response")
  coverage <- mean(ypred < pred[,2] & ypred > pred[,1], na.rm = T)
  cat("Coverage of 95% PI ", deparse(substitute(mod)), ":")
  
  return(coverage)
}

coverage(nb_gam_hour_lag, df_train_Arnulf_d1_hourly, pred_dist)
#
#prediction interval -----
#
pred_dist <- function(train_fit,
                      test_dat,
                      coef_perms,
                      nb_draws){
  require(mgcv)
  if(missing(nb_draws))
    nb_draws <- coef_perms
  ## get the mean and covariance matrix for predictions on the testing data
  test_preds <- predict(train_fit, test_dat, type = "lpmatrix")
  ## replicate parameter vectors
  rep_params <- rmvn(coef_perms,coef(train_fit),train_fit$Vp)
  pred_count <- matrix(0, nrow=nrow(test_dat), ncol=coef_perms)
  ## replicate predictions for each set of parameters
  for (i in 1:coef_perms) {
    rep_preds <- test_preds %*% rep_params[i,]
    pred_count[,i] <- exp(rep_preds)
  }
  ## find the dispersion parameter for the negative binomial
  r <- train_fit$family$getTheta(trans=TRUE)
  ## sample from the negative binomial distribution
  all_preds <- matrix(rnbinom(nrow(pred_count)*coef_perms*nb_draws,
                              mu=as.vector(pred_count), size=r),
                      nrow=nrow(test_dat)) %>%
    as.data.frame()
  all_preds <- apply(all_preds, 1, function(x) quantile(x,c(0.025,0.975), na.rm=T)) %>% t()
  return(all_preds)
}

pred_dist <- pred_dist(nb_gam_hour_lag, df_train_Arnulf_d1_hourly, 50, 50)


#nb_gam_min_no_lag-----
nb_gam_min_no_lag <-gam(y ~ 
                       s(air_temp, bs = "bs", k = 12) +
                       s(hour_weekday, bs = "cc", k = 50, by = as.factor(holiday)) +
                       s(month_year, bs = "cc", k = 50, by = ind),
                     family = nb(),
                     data = df_train_Arnulf_d1)

plot(nb_gam_min_no_lag, pages = 1, scale = 0)
summary(nb_gam_min_no_lag)

nb_gam_min_no_lag_PI <- pred_dist(nb_gam_min_no_lag, df_train_Arnulf_d1, 100, 100)
coverage(nb_gam_min_no_lag, df_train_Arnulf_d1, nb_gam_min_no_lag_PI)

# df_train_Arnulf_d1_hourly %>% 
#   mutate(y_pred = predict(nb_gam_min_no_lag, df_train_Arnulf_d1, type = "response")) %>%
#   mutate(lower = nb_gam_min_no_lag_PI[,1]) %>%
#   mutate(upper = nb_gam_min_no_lag_PI[,2]) %>%
#   ggplot(aes(x = 1:nrow(df_train_Arnulf_d1))) +
#   geom_point(aes(y = ypred,color = 'prediction'), alpha = 1) +
#   geom_ribbon(mapping = aes(ymin = lower, ymax= upper)) 

#nb_gam_min_lag-------------------

nb_gam_min_lag <-gam(y ~ 
                    s(air_temp, bs = "bs", k = 12) +
                    s(hour_weekday, bs = "cc", k = 50, by = as.factor(holiday)) +
                    s(month_year, bs = "cc", k = 50, by = ind) +
                    s(lag1, bs = "bs", k = 15),
                  family = nb(),
                  data = df_train_Arnulf_d1)

plot(nb_gam_min_lag, pages = 1, scale = 0)
summary(nb_gam_min_lag)

# nb_gam_min_lag_PI <- pred_dist(nb_gam_min_lag, df_train_Arnulf_d1, 100, 100)
# coverage(nb_gam_min_lag, df_train_Arnulf_d1, nb_gam_min_no_lag_PI)
# 
# df_train_Arnulf_d1_hourly %>% 
#   mutate(y_pred = predict(nb_gam_min_lag, df_train_Arnulf_d1, type = "response")) %>%
#   mutate(lower = nb_gam_min_lag_PI[,1]) %>%
#   mutate(upper = nb_gam_min_lag_PI[,2]) %>%
#   ggplot(aes(x = 1:nrow(df_train_Arnulf_d1))) +
#   geom_point(aes(y = ypred,color = 'prediction'), alpha = 1) +
#   geom_ribbon(mapping = aes(ymin = lower, ymax= upper)) 


#nb_gam_hour_no_lag-------------------
#hourly intervals
nb_gam_hour_no_lag <-gam(y ~ 
                        s(air_temp, bs = "bs", k = 12) +
                        s(hour_weekday, bs = "cc", k = 50, by = as.factor(holiday)) +
                        s(month_year, bs = "bs", k = 50, by = ind),
                      family = nb(),
                      data = df_train_Arnulf_d1_hourly)
mse(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly)
plot(nb_gam_hour_no_lag, pages = 1, scale = 0)
summary(nb_gam_hour_no_lag)

nb_gam_hour_no_lag_PI <- pred_dist(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly, 100, 100)
coverage(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly, nb_gam_hour_no_lag_PI)

df_train_Arnulf_d1_hourly %>% 
  mutate(y_pred = predict(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly, type = "response")) %>%
  mutate(lower = nb_gam_hour_no_lag_PI[,1]) %>%
  mutate(upper = nb_gam_hour_no_lag_PI[,2]) %>%
  ggplot(aes(x = 1:nrow(df_train_Arnulf_d1_hourly))) +
  geom_point(aes(y = ypred),color = 'red', alpha = 0.25) +
  geom_ribbon(mapping = aes(ymin = lower, ymax= upper), fill = "grey") 



#nb_gam_hour_no_lag_ind-------------------

nb_gam_hour_no_lag_ind <-gam(y ~ 
                            s(air_temp, bs = "bs", k = 12) +
                            s(hour_weekday, bs = "cc", k = 50, by = as.factor(holiday)) +
                            s(month_year, bs = "cc", k = 50) +
                            ind,
                          family = nb(),
                          data = df_train_Arnulf_d1_hourly)

plot(nb_gam_hour_no_lag_ind, pages = 1, scale = 0)
summary(nb_gam_hour_no_lag_ind)

nb_gam_hour_no_lag_ind_PI <- pred_dist(nb_gam_hour_no_lag_ind, df_train_Arnulf_d1_hourly, 100, 100)
coverage(nb_gam_hour_no_lag_ind, df_train_Arnulf_d1_hourly, nb_gam_hour_no_lag_ind_PI)

df_train_Arnulf_d1_hourly %>% 
  mutate(y_pred = predict(nb_gam_hour_no_lag_ind, df_train_Arnulf_d1_hourly, type = "response")) %>%
  mutate(lower = nb_gam_hour_no_lag_ind_PI[,1]) %>%
  mutate(upper = nb_gam_hour_no_lag_ind_PI[,2]) %>%
  ggplot(aes(x = 1:nrow(df_train_Arnulf_d1_hourly))) +
  geom_point(aes(y = ypred,color = 'prediction'), alpha = 1) +
  geom_ribbon(mapping = aes(ymin = lower, ymax= upper)) 

#nb_gam_hour_lag-------------------
#
nb_gam_hour_lag <-gam(y ~ 
                     s(air_temp, bs = "bs", k = 12) +
                     s(hour_weekday, bs = "cc", k = 50, by = as.factor(holiday)) +
                     s(month_year, bs = "cc", k = 50, by = ind) +
                     s(lag1, bs = "bs", k = 15),
                   family = nb(),
                   data = df_train_Arnulf_d1_hourly)

plot(nb_gam_hour_lag, pages = 1, scale = 0)
summary(nb_gam_hour_lag)

nb_gam_hour_lag_PI <- pred_dist(nb_gam_hour_lag, df_train_Arnulf_d1_hourly, 100, 100)
coverage(nb_gam_hour_lag, df_train_Arnulf_d1_hourly, nb_gam_hour_lag_PI)

df_train_Arnulf_d1_hourly %>% 
  mutate(y_pred = predict(nb_gam_hour_lag, df_train_Arnulf_d1_hourly, type = "response")) %>%
  mutate(lower = nb_gam_hour_lag_PI[,1]) %>%
  mutate(upper = nb_gam_hour_lag_PI[,2]) %>%
  ggplot(aes(x = 1:nrow(df_train_Arnulf_d1_hourly))) +
  geom_point(aes(y = ypred,color = 'prediction'), alpha = 1) +
  geom_ribbon(mapping = aes(ymin = lower, ymax= upper)) 
#plots----
df_train_Arnulf_d1_hourly %>% 
  mutate(y_pred = predict(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly, type = "response")) %>%
  ggplot(aes(x = 1:nrow(df_train_Arnulf_d1_hourly))) +
  #geom_point(aes(y = y,color = 'y'), alpha = 1) +
  geom_point(aes(y = y_pred, color = 'y_pred'), alpha = 0.5) +
  scale_color_manual(values = c("y" = "black", "y_pred" = "lightpink"),
                     labels = c("y", "fitted"))


#se CI----
df_train_Arnulf_d1_hourly %>% 
  mutate(se = predict(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly, type = "response", se.fit=T)$se.fit) %>% 
  mutate(fit = predict(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly, type = "response", se.fit=T)$fit)%>%
  ggplot(aes(x = 1:nrow(df_train_Arnulf_d1_hourly))) +
  geom_point(aes(y = y,color = 'y'), alpha = 1) +
  geom_ribbon(mapping = aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se)) 
  #scale_color_manual(values = c("y" = "black", "y_pred" = "lightpink"),
   #                  labels = c("y", "fitted"))

df_train_Arnulf_d1_hourly <- df_train_Arnulf_d1_hourly %>% 
  mutate(se = predict(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly, type = "response", se.fit=T)$se.fit) 
mean(df_train_Arnulf_d1_hourly$ypred < df_train_Arnulf_d1_hourly$ypred + 0.5*df_train_Arnulf_d1_hourly$se & df_train_Arnulf_d1_hourly$ypred > df_train_Arnulf_d1_hourly$ypred - 0.5*df_train_Arnulf_d1_hourly$se, na.rm = T)
#mse----
mse <- function(mod, df, type) { 
  pred <-  as.vector(predict(mod, df, type = "response"))
  mse <- mean((df$y - pred)^2, na.rm = TRUE)
  cat("MSE of ", deparse(substitute(mod)), ":")
  return(mse)
}

mse(gam_hour_lag, df_train_Arnulf_d1_hourly)
mse(gam_hour_no_lag, df_train_Arnulf_d1_hourly)
mse(gam_hour_no_lag_ind, df_train_Arnulf_d1_hourly)
mse(nb_gam_hour_lag, df_train_Arnulf_d1_hourly)
mse(nb_gam_hour_no_lag, df_train_Arnulf_d1_hourly)
mse(nb_gam_hour_no_lag_ind, df_train_Arnulf_d1_hourly)

mse(gam_min_no_lag, df_train_Arnulf_d1)
mse(gam_min_lag, df_train_Arnulf_d1)

#gam_min_no_lag------
gam_min_no_lag <-gam(y ~ 
                       s(air_temp, bs = "bs", k = 12) +
                       s(hour_weekday, bs = "cc", k = 50,by = as.factor(holiday)) +
                       s(month_year, bs = "cc", k = 50, by = ind),
                     family = quasipoisson,
                     data = df_train_Arnulf_d1)

plot(gam_min_no_lag, pages = 1, scale = 0)
summary(gam_min_no_lag)


#gam_min_lag-------------------
gam_min_lag <-gam(y ~ 
                    s(air_temp, bs = "bs", k = 12) +
                    s(hour_weekday, bs = "cc", k = 50,by = as.factor(holiday)) +
                    s(month_year, bs = "cc", k = 50, by = ind) +
                    s(lag1, bs = "bs", k = 15),
                  family = quasipoisson,
                  data = df_train_Arnulf_d1)

plot(gam_min_lag, pages = 1, scale = 0)
summary(gam_min_lag)

df_train_Arnulf_d1 %>% 
  mutate(y_pred = predict(gam_min_no_lag, df_train_Arnulf_d1, type = "response")) %>%
  ggplot(aes(x = 1:nrow(df_train_Arnulf_d1))) +
  geom_point(aes(y = y,color = 'y'), alpha = 1) +
  geom_point(aes(y = y_pred, color = 'y_pred'), alpha = 0.5) +
  scale_color_manual(values = c("y" = "black", "y_pred" = "lightpink"),
                     labels = c("y", "fitted"))
#gam_hour_no_lag-------------------
#hourly intervals
gam_hour_no_lag <-gam(y ~ 
                        s(air_temp, bs = "bs", k = 12) +
                        s(hour_weekday, bs = "cc", k = 50, by = as.factor(holiday)) +
                        s(month_year, bs = "cc", k = 50, by = ind),
                      family = quasipoisson,
                      data = df_train_Arnulf_d1_hourly)

plot(gam_hour_no_lag, pages = 1, scale = 0)
summary(gam_hour_no_lag)
#gam_hour_no_lag_ind-------------------
# gam_hour_no_lag_ind <-gam(y ~ 
#                        s(air_temp, bs = "bs", k = 12) +
#                        s(hour_weekday, bs = "cc", k = 50, by = as.factor(holiday)) +
#                        s(month_year, bs = "cc", k = 50) +
#                         ind,
#                      family = quasipoisson,
#                      data = df_train_Arnulf_d1_hourly)
# 
# plot(gam_hour_no_lag_ind, pages = 1, scale = 0)
# summary(gam_hour_no_lag_ind)

#gam_hour_lag-------------------
gam_hour_lag <-gam(y ~ 
                     s(air_temp, bs = "bs", k = 12) +
                     s(hour_weekday, bs = "cc", k = 50,by =  as.factor(holiday)) +
                     s(month_year, bs = "cc", k = 50, by = ind) +
                     s(lag1, bs = "bs", k = 15),
                   family = quasipoisson,
                   data = df_train_Arnulf_d1_hourly)

plot(gam_hour_lag, pages = 1, scale = 0)
summary(gam_hour_lag)

