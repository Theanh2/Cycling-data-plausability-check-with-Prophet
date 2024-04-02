#m_Arnulf_1_lag0_hourly-------------------
##
m_Arnulf_1_lag0_hourly <-gam(direction_1 ~ 
                           s(air_temp, bs = "bs", k = 20) +
                           s(hour_weekday, bs = "cc", k = 40) +
                           s(month_year, bs = "bs", k = 20) +
                           ind + holiday,
                         family = nb(),
                         data = df_Arnulf_hourly ,
                         method = "REML")


plot(m_Arnulf_1_lag0_hourly, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Arnulf_1_lag0_hourly)
#PI
spi_Arnulf_1_lag0_hourly <- simulation(model = m_Arnulf_1_lag0_hourly,
                                      data = df_Arnulf_hourly,
                                      direction = 1,
                                      n_sims = 1000,
                                      confidence = 0.995,
                                      seed = 1
                                      )

tpi_Arnulf_1_lag0_hourly <- theoryPI(model = m_Arnulf_1_lag0_hourly,
                                     data = df_Arnulf_hourly,
                                     direction = 1,
                                     confidence = 0.995
                                     )
#
#m_Arnulf_1_lag1_hourly-------------------
##
m_Arnulf_1_lag1_hourly <-gam(direction_1 ~ 
                        s(air_temp, bs = "bs", k = 20) +
                        s(hour_weekday, bs = "cc", k = 40) +
                        s(month_year, bs = "bs", k = 20) +
                        s(lag1_d1, bs = "bs", k = 20) +
                        ind + holiday,
                      family = nb(),
                      data = df_Arnulf_hourly ,
                      method = "REML")

plot(m_Arnulf_1_lag1_hourly, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Arnulf_1_lag1_hourly)
#PI
spi_Arnulf_1_lag1_hourly <- simulation(model = m_Arnulf_1_lag1_hourly,
                                       data = df_Arnulf_hourly,
                                       direction = 1,
                                       n_sims = 1000,
                                       confidence = 0.995,
                                       seed = 1
)

tpi_Arnulf_1_lag1_hourly <- theoryPI(model = m_Arnulf_1_lag1_hourly,
                                     data = df_Arnulf_hourly,
                                     direction = 1,
                                     confidence = 0.995
)
#
#m_Arnulf_1_lag0-----
##
m_Arnulf_1_lag0 <-gam(direction_1 ~ 
                          s(air_temp, bs = "bs", k = 20) +
                          s(hour_weekday, bs = "cc", k = 40) +
                          s(month_year, bs = "bs", k = 20) +
                          ind + holiday,
                        family = nb(),
                        data = df_Arnulf)

plot(m_Arnulf_1_lag0, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Arnulf_1_lag0)
#PI
spi_Arnulf_1_lag0 <- simulation(model = m_Arnulf_1_lag0,
                                       data = df_Arnulf,
                                       direction = 1,
                                       n_sims = 1000,
                                       confidence = 0.995,
                                       seed = 1
)

tpi_Arnulf_1_lag0 <- theoryPI(model = m_Arnulf_1_lag0,
                                     data = df_Arnulf,
                                     direction = 1,
                                     confidence = 0.995
)

#
#m_Arnulf_1_lag1-------------------
##
m_Arnulf_1_lag1 <-gam(direction_1 ~ 
                       s(air_temp, bs = "bs", k = 20) +
                       s(hour_weekday, bs = "cc", k = 40) +
                       s(month_year, bs = "bs", k = 20) +
                       s(lag1_d1, bs = "bs", k = 20) +
                       ind + holiday,
                     family = nb(),
                     data = df_Arnulf)

plot(m_Arnulf_1_lag1, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Arnulf_1_lag1)
#PI
spi_Arnulf_1_lag1 <- simulation(model = m_Arnulf_1_lag1,
                                       data = df_Arnulf,
                                       direction = 1,
                                       n_sims = 1000,
                                       confidence = 0.995,
                                       seed = 1
)

tpi_Arnulf_1_lag1 <- theoryPI(model = m_Arnulf_1_lag1,
                                     data = df_Arnulf,
                                     direction = 1,
                                     confidence = 0.995
)
#m_Arnulf_2_lag0_hourly-------------------
##
m_Arnulf_2_lag0_hourly <-gam(direction_2 ~ 
                               s(air_temp, bs = "bs", k = 20) +
                               s(hour_weekday, bs = "cc", k = 40) +
                               s(month_year, bs = "bs", k = 20) +
                               ind + holiday,
                             family = nb(),
                             data = df_Arnulf_hourly ,
                             method = "REML")


plot(m_Arnulf_2_lag0_hourly, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Arnulf_2_lag0_hourly)
#PI
spi_Arnulf_2_lag0_hourly <- simulation(model = m_Arnulf_2_lag0_hourly,
                                       data = df_Arnulf_hourly,
                                       direction = 2,
                                       n_sims = 1000,
                                       confidence = 0.995,
                                       seed = 1
)

tpi_Arnulf_2_lag0_hourly <- theoryPI(model = m_Arnulf_2_lag0_hourly,
                                     data = df_Arnulf_hourly,
                                     direction = 2,
                                     confidence = 0.995
)

#m_Arnulf_2_lag1_hourly-------------------
##
m_Arnulf_2_lag1_hourly <-gam(direction_2 ~ 
                               s(air_temp, bs = "bs", k = 20) +
                               s(hour_weekday, bs = "cc", k = 40) +
                               s(month_year, bs = "bs", k = 20) +
                               s(lag1_d2, bs = "bs", k = 20) +
                               ind + holiday,
                             family = nb(),
                             data = df_Arnulf_hourly ,
                             method = "REML")

plot(m_Arnulf_2_lag1_hourly, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Arnulf_2_lag1_hourly)
#PI
spi_Arnulf_2_lag1_hourly <- simulation(model = m_Arnulf_2_lag1_hourly,
                                       data = df_Arnulf_hourly,
                                       direction = 2,
                                       n_sims = 1000,
                                       confidence = 0.995,
                                       seed = 1
)

tpi_Arnulf_2_lag1_hourly <- theoryPI(model = m_Arnulf_2_lag1_hourly,
                                     data = df_Arnulf_hourly,
                                     direction = 2,
                                     confidence = 0.995
)
#
#m_Arnulf_2_lag0-----
##
m_Arnulf_2_lag0 <-gam(direction_2 ~ 
                        s(air_temp, bs = "bs", k = 20) +
                        s(hour_weekday, bs = "cc", k = 40) +
                        s(month_year, bs = "bs", k = 20) +
                        ind + holiday,
                      family = nb(),
                      data = df_Arnulf)

plot(m_Arnulf_2_lag0, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Arnulf_2_lag0)
#PI
spi_Arnulf_2_lag0 <- simulation(model = m_Arnulf_2_lag0,
                                data = df_Arnulf,
                                direction = 2,
                                n_sims = 1000,
                                confidence = 0.995,
                                seed = 1
                                )

tpi_Arnulf_2_lag0 <- theoryPI(model = m_Arnulf_2_lag0,
                              data = df_Arnulf,
                              direction = 2,
                              confidence = 0.995
                              )

#
#m_Arnulf_2_lag1-------------------
#
m_Arnulf_2_lag1 <-gam(direction_2 ~ 
                        s(air_temp, bs = "bs", k = 20) +
                        s(hour_weekday, bs = "cc", k = 40) +
                        s(month_year, bs = "bs", k = 20) +
                        s(lag1_d2, bs = "bs", k = 20) +
                        ind + holiday,
                      family = nb(),
                      data = df_Arnulf)

plot(m_Arnulf_2_lag1, pages = 1, scale = 0, all.terms = TRUE, trans = exp)
summary(m_Arnulf_2_lag1)
#PI
spi_Arnulf_2_lag1 <- simulation(model = m_Arnulf_2_lag1,
                                       data = df_Arnulf,
                                       direction = 2,
                                       n_sims = 1000,
                                       confidence = 0.995,
                                       seed = 1
)

tpi_Arnulf_2_lag1 <- theoryPI(model = m_Arnulf_2_lag1,
                                     data = df_Arnulf,
                                     direction = 2,
                                     confidence = 0.995
)
#save-----
save(m_Arnulf_1_lag0_hourly,
     spi_Arnulf_1_lag0_hourly,
     tpi_Arnulf_1_lag0_hourly,
     m_Arnulf_1_lag1_hourly,
     spi_Arnulf_1_lag1_hourly,
     tpi_Arnulf_1_lag1_hourly,
     m_Arnulf_1_lag0,
     spi_Arnulf_1_lag0,
     tpi_Arnulf_1_lag0,
     m_Arnulf_1_lag1,
     spi_Arnulf_1_lag1,
     tpi_Arnulf_1_lag1,
     m_Arnulf_2_lag0_hourly,
     spi_Arnulf_2_lag0_hourly,
     tpi_Arnulf_2_lag0_hourly,
     m_Arnulf_2_lag1_hourly,
     spi_Arnulf_2_lag1_hourly,
     tpi_Arnulf_2_lag1_hourly,
     m_Arnulf_2_lag0,
     spi_Arnulf_2_lag0,
     tpi_Arnulf_2_lag0,
     m_Arnulf_2_lag1,
     spi_Arnulf_2_lag1,
     tpi_Arnulf_2_lag1,
     file = "Arnulf.RData")

# rm(m_Arnulf_1_lag0_hourly,
#    spi_Arnulf_1_lag0_hourly,
#    tpi_Arnulf_1_lag0_hourly,
#    m_Arnulf_1_lag1_hourly,
#    spi_Arnulf_1_lag1_hourly,
#    tpi_Arnulf_1_lag1_hourly,
#    m_Arnulf_1_lag0,
#    spi_Arnulf_1_lag0,
#    tpi_Arnulf_1_lag0,
#    m_Arnulf_1_lag1,
#    spi_Arnulf_1_lag1,
#    tpi_Arnulf_1_lag1,
#    m_Arnulf_2_lag0_hourly,
#    spi_Arnulf_2_lag0_hourly,
#    tpi_Arnulf_2_lag0_hourly,
#    m_Arnulf_2_lag1_hourly,
#    spi_Arnulf_2_lag1_hourly,
#    tpi_Arnulf_2_lag1_hourly,
#    m_Arnulf_2_lag0,
#    spi_Arnulf_2_lag0,
#    tpi_Arnulf_2_lag0,
#    m_Arnulf_2_lag1,
#    spi_Arnulf_2_lag1,
#    tpi_Arnulf_2_lag1)
