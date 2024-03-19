#--------------------#
#----Load Packages----
#--------------------#

# devtools::install_github("nicholasjclark/mvgam")
library(mvgam)
library(dplyr)
library(ggplot2)
library(gratia)
library(marginaleffects)
library(tidybayes)

#---------------------------------------#
#---Modeling with Bayesian Approaches----
#---------------------------------------#

# de
# A custom ggplot2 theme
theme_set(
  theme_classic(base_size = 10, base_family = "serif") +
    theme(axis.line.x.bottom = element_line(color = 'black', size = 0.5),
          axis.line.y.left = element_line(color = 'black', size = 0.5))
)

options(
  ggplot2.discrete.color = c("#A25050",
                             "#8F2727",
                             "darkred",
                             "#630000"),
  ggplot2.discrete.fill = c("#A25050",
                            "#8F2727",
                            "darkred",
                            "#630000")
)



dat <- sim_mvgam(T = 100, n_series = 6, n_lv = 2,
                 family = 'poisson',
                 mu = runif(6, -1, 2),
                 trend_rel = 0.6)
head(dat$data_train)

set.seed(1111)
trend <- cumsum(rnorm(53, -0.1, 0.5))
observations <- rnbinom(53, size = 10, mu = 4 + exp(trend))
data.frame(y = observations) %>%
  dplyr::mutate(lag1 = lag(y),
                lag2 = lag(y, 2),
                lag3 = lag(y, 3)) %>%
  dplyr::slice_tail(n = 50)

Sys.time()
m <- mvgam(
  formula = y ~ s(year) + s(month) + s(hour_weekday) + s(air_temp) + s(precipitation),
  data = d1,
  newdata = d2,
  family = 'nb',
  trend_model = "AR1",
  chains = 2,
  burnin = 1000
)
Sys.time()


d1 = df_train_Arnulf_d1
d2 = df_test_Arnulf_d1

d1 %>% slice(which(!(is.na(d1$air_temp) | is.na(d1$precipitation)))) -> d1
d2 %>% slice(which(!(is.na(d2$air_temp) | is.na(d2$precipitation)))) -> d2

library(mgcv)
m <- bam(
  y ~ s(year) + s(month) + s(hour_weekday) + s(air_temp) + s(precipitation),
  data = df_train_Arnulf_d1,
  family = 'nb'
)

df_train_Arnulf_d1 %>% 
  mutate(air_temp = ifelse(is.na(air_temp),
                           ifelse(year == 2016, 8.0, air_temp), air_temp)) %>%
  filter(year==2010, month==1, day==2, hour==13) %>%
  select(air_temp)
 

df_train_Arnulf_d1 %>% count(y) %>% mutate(p = n/sum(n))


library(brms) # 16:46:44 KST"
Sys.time()
m <- brm(
  y ~ s(year) + s(hour_weekday, bs = 'cc') + s(air_temp),
  data = df_train_Arnulf_d1,
  control = list(adapt_delta = 0.9, max_treedepth = 10),
  chains = 1,
  cores = 8,
  family = zero_inflated_negbinomial(link = "log", link_shape = "log", link_zi = "logit"),
  file = 'test',
  iter = 600,
  warmup = 300)
Sys.time() # 14:34:45 KST"
# Chain 1:  Elapsed Time: 15262.8 seconds (Warm-up)
# Chain 1:                63185 seconds (Sampling)
# Chain 1:                78447.8 seconds (Total)

summary(m)
ms <- marginal_smooths(m)
plot(ms)



library(mgcv)



m = gamm(
  y ~ s(year, by = ind) + s(month) + s(hour_weekday, by = holiday, k = 30) + s(air_temp, k = 30),
  data = df_train_Arnulf_d1,
  family = quasipoisson()
)
#3:29 - 5:00
summary(m$gam)
acf(resid(m$lme, type = "normalized"))
pacf(resid(m$lme, type = "normalized"))

# 5:13
m_lagged = gamm(
  y ~ s(year, by = ind) + s(month) + s(hour_weekday, by = holiday, k = 30) + s(air_temp, k = 30) +
    s(lag1) + s(lag2) + s(lag3) + s(lag4) + s(lag5),
  family = quasipoisson(),
  data = 
    df_train_Arnulf_d1 %>%
    dplyr::mutate(lag1 = lag(y, 1),
                  lag2 = lag(y, 2),
                  lag3 = lag(y, 3),
                  lag4 = lag(y, 4),
                  lag5 = lag(y, 5))
)


acf(resid(m_lagged$lme, type = "normalized"))
pacf(resid(m_lagged$lme, type = "normalized"))

p = predict(object = m$gam, newdata = df_test_Arnulf_d1)
summary(exp(p))
p_lagged = predict(object = m_lagged$gam, newdata = df_test_Arnulf_d1 %>%
                     dplyr::mutate(lag1 = lag(y, 1),
                                   lag2 = lag(y, 2),
                                   lag3 = lag(y, 3),
                                   lag4 = lag(y, 4),
                                   lag5 = lag(y, 5)))
summary(exp(p_lagged))
summary(m_lagged$gam)
df_test_Arnulf_d1 %>% 
  mutate(y_pred = exp(p), y_pred_lagged = exp(p_lagged)) %>%
  ggplot(aes(x = ds)) + 
  geom_point(aes(y = y,color = 'y'), alpha = 0.5) +
  geom_point(aes(y = y_pred, color = 'y_pred'), alpha = 0.5) +
  geom_point(aes(y = y_pred_lagged, color = 'y_pred_lagged'), alpha = 0.5) +
  scale_color_manual(values = c("y" = "black", "y_pred" = "lightpink", "y_pred_lagged" = "lightgreen"),
                     labels = c("y", "wihout lag", "with lag(5)"))


df_test_Arnulf_d1 %>% slice(which.max(p))

gam.check(m_lagged$gam)

plot(m_lagged$gam, pages = 1, scale = FALSE)


df_train_Arnulf_d1 %>% 
  group_by(year, month, day, hour) %>%
  summarise(tt = sum(y, na.rm = TRUE))


df_temp_precip %>% 
  mutate(year_month = (year-2008) + (month-6)) %>%
  filter(year == 2009, month == 12)

# 2008 6 - 0
# 2008 7 - 1
# 2008 8 - 2
# 2008 9 - 3
# 2008 10 - 4
# 2008 11 -5
# 2008 12 - 6
# 2009 1 - 7
# 2009 2 - 8
# 2009 3- 9
# 2009 4 - 10
# 2009 5 - 11
# 2009 6 - 12
...

# 2009 12 - 18
# 20
