#m_Hirsch_1_lag0_hourly-------------------
##
m_Hirsch_1_lag0_hourly <-gam(direction_1 ~ 
                                s(air_temp, bs = "bs", k = 20) +
                                s(hour_weekday, bs = "cc", k = 40) +
                                s(month_year, bs = "bs", k = 20) +
                                ind + holiday,
                              family = nb(),
                              data = df_Hirsch_hourly ,
                              method = "REML")


plot(m_Hirsch_1_lag0_hourly, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Hirsch_1_lag0_hourly)
#PI
spi_Hirsch_1_lag0_hourly <- simulation(model = m_Hirsch_1_lag0_hourly,
                                        data = df_Hirsch_hourly,
                                        direction = 1,
                                        n_sims = 1000,
                                        confidence = 0.995,
                                        seed = 1
)

tpi_Hirsch_1_lag0_hourly <- theoryPI(model = m_Hirsch_1_lag0_hourly,
                                      data = df_Hirsch_hourly,
                                      direction = 1,
                                      confidence = 0.995
)
#
#m_Hirsch_1_lag1_hourly-------------------
##
m_Hirsch_1_lag1_hourly <-gam(direction_1 ~ 
                                s(air_temp, bs = "bs", k = 20) +
                                s(hour_weekday, bs = "cc", k = 40) +
                                s(month_year, bs = "bs", k = 20) +
                                s(lag1_d1, bs = "bs", k = 20) +
                                ind + holiday,
                              family = nb(),
                              data = df_Hirsch_hourly ,
                              method = "REML")

plot(m_Hirsch_1_lag1_hourly, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Hirsch_1_lag1_hourly)
#PI
spi_Hirsch_1_lag1_hourly <- simulation(model = m_Hirsch_1_lag1_hourly,
                                        data = df_Hirsch_hourly,
                                        direction = 1,
                                        n_sims = 1000,
                                        confidence = 0.995,
                                        seed = 1
)

tpi_Hirsch_1_lag1_hourly <- theoryPI(model = m_Hirsch_1_lag1_hourly,
                                      data = df_Hirsch_hourly,
                                      direction = 1,
                                      confidence = 0.995
)
#
#m_Hirsch_1_lag0-----
##
m_Hirsch_1_lag0 <-gam(direction_1 ~ 
                         s(air_temp, bs = "bs", k = 20) +
                         s(hour_weekday, bs = "cc", k = 40) +
                         s(month_year, bs = "bs", k = 20) +
                         ind + holiday,
                       family = nb(),
                       data = df_Hirsch)

plot(m_Hirsch_1_lag0, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Hirsch_1_lag0)
#PI
spi_Hirsch_1_lag0 <- simulation(model = m_Hirsch_1_lag0,
                                 data = df_Hirsch,
                                 direction = 1,
                                 n_sims = 1000,
                                 confidence = 0.995,
                                 seed = 1
)

tpi_Hirsch_1_lag0 <- theoryPI(model = m_Hirsch_1_lag0,
                               data = df_Hirsch,
                               direction = 1,
                               confidence = 0.995
)

#
#m_Hirsch_1_lag1-------------------
##
m_Hirsch_1_lag1 <-gam(direction_1 ~ 
                         s(air_temp, bs = "bs", k = 20) +
                         s(hour_weekday, bs = "cc", k = 40) +
                         s(month_year, bs = "bs", k = 20) +
                         s(lag1_d1, bs = "bs", k = 20) +
                         ind + holiday,
                       family = nb(),
                       data = df_Hirsch)

plot(m_Hirsch_1_lag1, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Hirsch_1_lag1)
#PI
spi_Hirsch_1_lag1 <- simulation(model = m_Hirsch_1_lag1,
                                 data = df_Hirsch,
                                 direction = 1,
                                 n_sims = 1000,
                                 confidence = 0.995,
                                 seed = 1
)

tpi_Hirsch_1_lag1 <- theoryPI(model = m_Hirsch_1_lag1,
                               data = df_Hirsch,
                               direction = 1,
                               confidence = 0.995
)
#m_Hirsch_2_lag0_hourly-------------------
##
m_Hirsch_2_lag0_hourly <-gam(direction_2 ~ 
                                s(air_temp, bs = "bs", k = 20) +
                                s(hour_weekday, bs = "cc", k = 40) +
                                s(month_year, bs = "bs", k = 20) +
                                ind + holiday,
                              family = nb(),
                              data = df_Hirsch_hourly ,
                              method = "REML")


plot(m_Hirsch_2_lag0_hourly, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Hirsch_2_lag0_hourly)
#PI
spi_Hirsch_2_lag0_hourly <- simulation(model = m_Hirsch_2_lag0_hourly,
                                        data = df_Hirsch_hourly,
                                        direction = 2,
                                        n_sims = 1000,
                                        confidence = 0.995,
                                        seed = 1
)

tpi_Hirsch_2_lag0_hourly <- theoryPI(model = m_Hirsch_2_lag0_hourly,
                                      data = df_Hirsch_hourly,
                                      direction = 2,
                                      confidence = 0.995
)

#m_Hirsch_2_lag1_hourly-------------------
##
m_Hirsch_2_lag1_hourly <-gam(direction_2 ~ 
                                s(air_temp, bs = "bs", k = 20) +
                                s(hour_weekday, bs = "cc", k = 40) +
                                s(month_year, bs = "bs", k = 20) +
                                s(lag1_d2, bs = "bs", k = 20) +
                                ind + holiday,
                              family = nb(),
                              data = df_Hirsch_hourly ,
                              method = "REML")

plot(m_Hirsch_2_lag1_hourly, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Hirsch_2_lag1_hourly)
#PI
spi_Hirsch_2_lag1_hourly <- simulation(model = m_Hirsch_2_lag1_hourly,
                                        data = df_Hirsch_hourly,
                                        direction = 2,
                                        n_sims = 1000,
                                        confidence = 0.995,
                                        seed = 1
)

tpi_Hirsch_2_lag1_hourly <- theoryPI(model = m_Hirsch_2_lag1_hourly,
                                      data = df_Hirsch_hourly,
                                      direction = 2,
                                      confidence = 0.995
)
#
#m_Hirsch_2_lag0-----
##
m_Hirsch_2_lag0 <-gam(direction_2 ~ 
                         s(air_temp, bs = "bs", k = 20) +
                         s(hour_weekday, bs = "cc", k = 40) +
                         s(month_year, bs = "bs", k = 20) +
                         ind + holiday,
                       family = nb(),
                       data = df_Hirsch)

plot(m_Hirsch_2_lag0, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Hirsch_2_lag0)
#PI
spi_Hirsch_2_lag0 <- simulation(model = m_Hirsch_2_lag0,
                                 data = df_Hirsch,
                                 direction = 2,
                                 n_sims = 1000,
                                 confidence = 0.995,
                                 seed = 1
)

tpi_Hirsch_2_lag0 <- theoryPI(model = m_Hirsch_2_lag0,
                               data = df_Hirsch,
                               direction = 2,
                               confidence = 0.995
)

#
#m_Hirsch_2_lag1-------------------
#
m_Hirsch_2_lag1 <-gam(direction_2 ~ 
                         s(air_temp, bs = "bs", k = 20) +
                         s(hour_weekday, bs = "cc", k = 40) +
                         s(month_year, bs = "bs", k = 20) +
                         s(lag1_d2, bs = "bs", k = 20) +
                         ind + holiday,
                       family = nb(),
                       data = df_Hirsch)

plot(m_Hirsch_2_lag1, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Hirsch_2_lag1)
#PI
spi_Hirsch_2_lag1 <- simulation(model = m_Hirsch_2_lag1,
                                 data = df_Hirsch,
                                 direction = 2,
                                 n_sims = 1000,
                                 confidence = 0.995,
                                 seed = 1
)

tpi_Hirsch_2_lag1 <- theoryPI(model = m_Hirsch_2_lag1,
                               data = df_Hirsch,
                               direction = 2,
                               confidence = 0.995
)
#save-----
save(m_Hirsch_1_lag0_hourly,
     spi_Hirsch_1_lag0_hourly,
     tpi_Hirsch_1_lag0_hourly,
     m_Hirsch_1_lag1_hourly,
     spi_Hirsch_1_lag1_hourly,
     tpi_Hirsch_1_lag1_hourly,
     m_Hirsch_1_lag0,
     spi_Hirsch_1_lag0,
     tpi_Hirsch_1_lag0,
     m_Hirsch_1_lag1,
     spi_Hirsch_1_lag1,
     tpi_Hirsch_1_lag1,
     m_Hirsch_2_lag0_hourly,
     spi_Hirsch_2_lag0_hourly,
     tpi_Hirsch_2_lag0_hourly,
     m_Hirsch_2_lag1_hourly,
     spi_Hirsch_2_lag1_hourly,
     tpi_Hirsch_2_lag1_hourly,
     m_Hirsch_2_lag0,
     spi_Hirsch_2_lag0,
     tpi_Hirsch_2_lag0,
     m_Hirsch_2_lag1,
     spi_Hirsch_2_lag1,
     tpi_Hirsch_2_lag1,
     file = "Hirsch.RData")

rm(m_Hirsch_1_lag0_hourly,
   spi_Hirsch_1_lag0_hourly,
   tpi_Hirsch_1_lag0_hourly,
   m_Hirsch_1_lag1_hourly,
   spi_Hirsch_1_lag1_hourly,
   tpi_Hirsch_1_lag1_hourly,
   m_Hirsch_1_lag0,
   spi_Hirsch_1_lag0,
   tpi_Hirsch_1_lag0,
   m_Hirsch_1_lag1,
   spi_Hirsch_1_lag1,
   tpi_Hirsch_1_lag1,
   m_Hirsch_2_lag0_hourly,
   spi_Hirsch_2_lag0_hourly,
   tpi_Hirsch_2_lag0_hourly,
   m_Hirsch_2_lag1_hourly,
   spi_Hirsch_2_lag1_hourly,
   tpi_Hirsch_2_lag1_hourly,
   m_Hirsch_2_lag0,
   spi_Hirsch_2_lag0,
   tpi_Hirsch_2_lag0,
   m_Hirsch_2_lag1,
   spi_Hirsch_2_lag1,
   tpi_Hirsch_2_lag1)