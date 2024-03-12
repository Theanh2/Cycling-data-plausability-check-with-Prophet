#----------------------------#
#----Set working directory----
#----------------------------#
# setwd("/Users/theanh/Library/Mobile Documents/com~apple~CloudDocs/Stats/Master/Consulting")

#--------------------#
#----Load Packages----
#--------------------#
library(dplyr)
library(data.table)
library(tidyr)
library(lubridate)
library(stringr)
library(mgcv)
library(ggplot2)
library(prophet)
library(prophetuneR)

# #------------------------#
# #----Utility functions----
# #------------------------#
# # Extract hour from the time string
# extract_hour = function(x) {
#   return(format(strptime(x, format = "%H:%M:%S"), format = "%H"))
# }
# 
# # Extract month from the time string
# extract_month = function(x) {
#   return(as.integer(format(x, "%m")))
# }
# 
# # Extract year from the time string
# extract_year = function(x) {
#   return(as.integer(substr(x, 1, 4)))
# }
# 
# # Hourly weather data
# # Extract year, month, day and hour from the weather data frame
extract_DateTime = function(df) {

  df$date = as.character(df$date)
  df$hour = as.integer(substr(df$date, 9, 10))
  df$year = as.integer(substr(df$date, 1, 4))
  df$month = as.integer(substr(df$date, 5, 6))
  df$day = as.integer(substr(df$date, 7, 8))

  return(df)
}
# 
# extract_yearmonthdayhour = function(df) {
#   
#   temp = df$ds
#   df$hour = as.integer(substr(temp, 12, 13))
#   df$year = as.integer(substr(temp, 1, 4))
#   df$month = as.integer(substr(temp, 6, 7))
#   df$day = as.integer(substr(temp, 9, 10))
#   df$hour[is.na(df$hour)] <- 0
#   return(df)
#   
# }



# Note that you don't have to the following process from scratch to load cycling data.
# Instead, just import it from `data_folder`
getwd() # adjust the path 

df_cycling <- data.table::fread("/Users/juntaekwon/work_r/Consulting Project/data_folder/df_cycling.csv") 
idx <- sapply(df_cycling$ds, function(x) ifelse(nchar(x) == 19, FALSE, TRUE), USE.NAMES = FALSE) 
df_cycling[idx,]$ds <- paste(df_cycling[idx,]$ds, ":00", sep = "") # to unify the format of ds column

# Replace empty strings with NA in comment
df_cycling$comment <- ifelse(df_cycling$comment == "", NA, df_cycling$comment)
df_cycling %>% distinct(comment)

# Note that Monday(dayofweek == 0) through Sunday(dayofweek == 6)
df_cycling %>% distinct(dayofweek)

# Modify some erroneous values e.g. "2020-04-09 13:59:00" -> "2020-04-09 14:00:00"

# Kreuther
df_cycling <- df_cycling %>%
  mutate(
    ds = if_else(station == "Kreuther" & ds == "2020-04-09 13:59:00", "2020-04-09 14:00:00", ds),
    hour = if_else(station == "Kreuther" & ds == "2020-04-09 14:00:00" & hour == 13, 14, hour),
    hour_weekday = if_else(station == "Kreuther" & ds == "2020-04-09 14:00:00" & hour_weekday == 85, 86, hour_weekday)
  )

df_cycling <- df_cycling %>%
  mutate(
    ds = if_else(station == "Kreuther" & ds == "2020-04-09 14:14:00", "2020-04-09 14:15:00", ds)
  )

df_cycling <- df_cycling %>%
  mutate(
    ds = if_else(station == "Kreuther" & ds == "2020-04-09 14:29:00", "2020-04-09 14:30:00", ds)
  )

df_cycling <- df_cycling %>%
  mutate(
    ds = if_else(station == "Kreuther" & ds == "2020-04-09 14:44:00", "2020-04-09 14:45:00", ds)
  )

df_cycling <- df_cycling %>%
  mutate(
    ds = if_else(station == "Kreuther" & ds == "2020-04-09 14:59:00", "2020-04-09 15:00:00", ds),
    hour = if_else(station == "Kreuther" & ds == "2020-04-09 15:00:00" & hour == 14, 15, hour),
    hour_weekday = if_else(station == "Kreuther" & ds == "2020-04-09 15:00:00" & hour_weekday == 86, 87, hour_weekday)
  )

# Olympia
insert_df_Olympia <- df_cycling %>% 
  filter(station == "Olympia", date == "2017-11-22", hour == 14) %>% 
  slice(1) %>% 
  mutate(ds = "2017-11-22 14:00:00",
         direction_1 = NA,
         direction_2 = NA)

df_cycling <- bind_rows(df_cycling, insert_df_Olympia)

# Hirsch
idx_remove <- c(
  which(df_cycling$station == "Hirsch" & df_cycling$ds == "2010-03-28 02:00:00")[1],
  which(df_cycling$station == "Hirsch" & df_cycling$ds == "2010-03-28 02:15:00")[1],
  which(df_cycling$station == "Hirsch" & df_cycling$ds == "2010-03-28 02:30:00")[1],
  which(df_cycling$station == "Hirsch" & df_cycling$ds == "2010-03-28 02:45:00")[1]
)
df_cycling <- df_cycling %>% filter(row_number() %notin% idx_remove)

insert_df_Hirsch_1 <- df_cycling %>% 
  filter(station == "Hirsch", date == "2019-05-23", hour == 9) %>% 
  slice(1) %>% 
  mutate(ds = "2019-05-23 09:00:00",
         direction_1 = NA,
         direction_2 = NA)

insert_df_Hirsch_2 <- df_cycling %>% 
  filter(station == "Hirsch", date == "2019-05-29", hour == 7) %>% 
  slice(1) %>% 
  mutate(ds = "2019-05-23 07:00:00",
         direction_1 = NA,
         direction_2 = NA)

df_cycling <- bind_rows(df_cycling, insert_df_Hirsch_1, insert_df_Hirsch_2)

# Erhardt
insert_df_Erhardt_1 <- df_cycling %>% 
  filter(station == "Erhardt", date == "2017-11-28", hour == 13) %>% 
  slice(1) %>% 
  mutate(ds = "2017-11-22 13:00:00",
         direction_1 = NA,
         direction_2 = NA)

insert_df_Erhardt_2 <- df_cycling %>% 
  filter(station == "Erhardt", date == "2017-11-28", hour == 13) %>% 
  slice(1) %>% 
  mutate(ds = "2017-11-22 13:15:00",
         direction_1 = NA,
         direction_2 = NA)

insert_df_Erhardt_3 <- df_cycling %>% 
  filter(station == "Erhardt", date == "2017-11-28", hour == 13) %>% 
  slice(1) %>% 
  mutate(ds = "2017-11-22 13:30:00",
         direction_1 = NA,
         direction_2 = NA)

insert_df_Erhardt_4 <- df_cycling %>% 
  filter(station == "Erhardt", date == "2017-11-28", hour == 15) %>% 
  slice(1) %>% 
  mutate(ds = "2017-11-22 15:00:00",
         direction_1 = NA,
         direction_2 = NA)

insert_df_Erhardt_5 <- df_cycling %>% 
  filter(station == "Erhardt", date == "2017-11-28", hour == 15) %>% 
  slice(1) %>% 
  mutate(ds = "2017-11-22 15:15:00",
         direction_1 = NA,
         direction_2 = NA)

df_cycling <- bind_rows(df_cycling,
                        insert_df_Erhardt_1,
                        insert_df_Erhardt_2,
                        insert_df_Erhardt_3,
                        insert_df_Erhardt_4,
                        insert_df_Erhardt_5)

# Ascending order of ds
  
  

#---------------------------------------------#
#----Load and Reformat cycling/weather data----
#---------------------------------------------#
# cycling_url = c(
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/66be7619-a672-4382-bf88-e3688c5abc2b/download/rad_2008_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/3ef8aad9-a6b0-4c97-a6b7-8c3a63226b37/download/rad_2009_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/6558a5f9-2c96-4e4b-985d-8eb99b7b73b1/download/rad_2010_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/86962013-4854-4deb-aaf9-36e3770cde24/download/rad_2011_15min_export_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/ff5d2ebf-dde6-4f21-9c68-2aab74addeec/download/rad_2012_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/9f4f798f-0ad1-4e86-8157-15c5e46eaf91/download/rad_2013_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/784b925b-1d5f-43d3-8353-fd5d02fc7c53/download/rad_2014_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/d6b3a72c-b180-40a0-a2ab-d97040737f20/download/rad_2015_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/3913d9e6-1be8-4ee4-ab88-1266cbf161f1/download/rad_2016_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/694b9927-b4d5-4e8f-9c62-09b8ac03c39a/download/rad_2017_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/0a97a624-daa4-4cd8-a820-7d2fa6ffe89a/download/rad_2018_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/893e1f16-6504-4f4f-b8b3-f907ef406cd5/download/rad_2019_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/9be77b23-a444-4ba5-be9e-7f8594aa0188/download/rad_2020_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/205c5c9e-9689-4c28-97cb-e575c6c772ce/download/rad_2021_15min_06_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/e65e0e26-ce1f-4e58-9260-beccac196e75/download/rad_2022_15min_06_06_23_r.csv"
# )
# 
# weather_url = c(
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/53ef8c4b-d411-477f-9cf3-b044a4c1aaaa/download/rad_2008_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/89dbef6c-c6cb-4bfc-8729-93049e91223d/download/rad_2009_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/cb720004-bcde-4c0e-babf-64ee995c5232/download/rad_2010_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/8c752f92-2cb3-4765-aecf-bda349592ab2/download/rad_2011_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/7304e087-e02d-4ca1-b4da-5c46b27fa223/download/rad_2012_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/e281aac1-c476-48da-84ce-2bf52bf20a08/download/rad_2013_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/b78e7a05-33a5-422c-98fa-27c5cf9afe81/download/rad_2014_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/d5dd6fda-77ee-4ac7-9c7f-af54ccea64e4/download/rad_2015_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/2f9e99cf-e82d-41fb-990c-67783cf23ab7/download/rad_2016_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/d584bb5e-021b-43ea-8d32-8ba6c92b8f1e/download/rad_2017_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/00c5eaf9-d464-433f-8c9b-ce8a2a16db2b/download/rad_2018_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/f6d559cc-9e30-4307-b005-7b105b967ec0/download/rad_2019_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/8ae44963-171e-4edf-bf16-f12503717042/download/rad_2020_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/561fb0d5-2d27-41bb-bda9-a383d6d42ad1/download/rad_2021_tage_19_06_23_r.csv",
#   "https://opendata.muenchen.de/dataset/022a11ff-4dcb-4f03-b7dd-a6c94a094587/resource/05a2178d-3138-4874-a9fd-1ede6f0cedc1/download/rad_2022_tage_19_06_23_r.csv"
# )
# 
# cycling_ls = vector(mode = "list", length = length(cycling_url))
# weather_ls = vector(mode = "list", length = length(weather_url))
# for (i in seq_len(length(cycling_ls))) {
#   cycling_ls[[i]] = data.table::fread(cycling_url[i])
#   weather_ls[[i]] = data.table::fread(weather_url[i])
# }
# 
# df_cycling = do.call(rbind, cycling_ls)
# df_weather = do.call(rbind, weather_ls)
# 
# #--------------------------#
# #----Reformat df_cycling----
# #--------------------------#
# # mutate date and uhrzeit_start into YYYY-MM-DD HH:MM:SS for prophet
# df_cycling <- df_cycling %>% mutate(ds = as.character(paste(datum, uhrzeit_start)))
# 
# # Extract date
# df_cycling$hour <- as.integer(extract_hour(df_cycling$uhrzeit_start))
# df_cycling$year <- as.integer(format(df_cycling$datum, "%Y"))
# df_cycling$month <- as.integer(format(df_cycling$datum, "%m"))
# df_cycling$weekday <- weekdays(df_cycling$datum)
# df_cycling$day <- as.integer(format(df_cycling$datum, "%d"))
# 
# names(df_cycling) <- c(datum = "date",
#                        uhrzeit_start ="time_start",
#                        uhrzeit_ende = "time_end",
#                        zaehlstelle = "station",
#                        richtung_1 = "direction_1",
#                        richtung_2 = "direction_2",
#                        gesamt= "total",
#                        kommentar = "comment",
#                        ds ="ds",
#                        hour = "hour", 
#                        year = "year",
#                        month = "month",
#                        weekday = "weekday",
#                        day = "day")
# 
# df_cycling$comment[which(df_cycling$comment == "Zählstelle noch nicht in Betrieb")] <- "station not yet in operation"
# df_cycling$comment[which(df_cycling$comment == "Radweg vereist / nach Schneefall nicht geräumt / keine Messung möglich")] <- "bikelane icy / not cleared after snowfall / no measure possible"
# df_cycling$comment[which(df_cycling$comment == "Baustelle")] <- "construction"
# df_cycling$comment[which(df_cycling$comment == "Austausch Sensor")] <- "replacing sensor"
# df_cycling$comment[which(df_cycling$comment == "Ausfall nach Beschädigung")] <- "failure after damage"
# 
# # Add hour_weekday
# df_cycling$weekday <- factor(df_cycling$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
# df_cycling <- df_cycling %>% mutate(hour_weekday = (as.integer((weekday)) - 1)*24 + hour)
# 
# # Remove rows where direction_1 or direction_2 == -1
# idx_remove <- which(df_cycling$direction_1 == -1 | df_cycling$direction_2 == -1)
# df_cycling <- df_cycling[-idx_remove,]
# 
# #----------------------------------#
# #----Reformat df_weather (daily)----
# #----------------------------------#
# df_weather$year <- as.integer(format(df_weather$datum, "%Y"))
# names(df_weather) <- c("date", "station", "time_start", "time_end", "direction_1", "direction_2", "total", "min.temp", "max.temp", "precipitation", "cloud_cover", "sun_hours", "comment", year = "year")
# 
# df_weather$comment[which(df_weather$comment == "Zählstelle noch nicht in Betrieb")] <- "Station not yet in operation"
# df_weather$comment[which(df_weather$comment == "Radweg vereist / nach Schneefall nicht geräumt / keine Messung möglich")] <- "bikelane icy / not cleared after snowfall / no measure possible"
# df_weather$comment[which(df_weather$comment == "Ausfall")] <- "failure"
# df_weather$comment[which(df_weather$comment == "Baustelle")] <- "construction"
# df_weather$comment[which(df_weather$comment == "Austausch Sensor")] <- "replacing sensor"
# df_weather$comment[which(df_weather$comment == "Ausfall nach Beschädigung")] <- "failure after damage"

#-----------------------#
#----Load holiday set----
#-----------------------#
school_holidays <- read.table(
  "data_folder/holidays_data/school_holidays.csv",
  sep = ";", header = TRUE) 

public_holidays <- read.table(
  "data_folder/holidays_data/public_holidays.csv",
  sep = ";", header = TRUE)

school_holidays$date <- as.Date(school_holidays$date, format = "%d.%m.%y")
public_holidays$date <- as.Date(public_holidays$date, format = "%Y-%m-%d")
colnames(public_holidays)[1] <- "name_holiday"
public_holidays <- public_holidays %>% select("date", "public_holiday")
public_holidays <- public_holidays %>% mutate(public_holiday = ifelse(public_holiday == 2, 1, public_holiday)) # Make holiday binary

# From prophet package
holiday <- generated_holidays %>% filter(country == "DE")

#---------------------------------------------------#
# Load and Reformat weather data from DWD (hourly----
#---------------------------------------------------#
# 1. Air temperature
# Description: See Metadaten_Parameter_tu_stunde_03379.html in the folder / https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/air_temperature/DESCRIPTION_obsgermany_climate_hourly_air_temperature_en.pdf
# Station_id: 03379; München Stadt
# Mess_datum: reference date
# QN_9: quality level
# TT_TU: air temperature
# RF_TU relative humidity
air_temp <- read.table(
  "data_folder/weather_data/air_temperature/produkt_tu_stunde_19970701_20221231_03379.txt",
  sep = ";", header = TRUE)

# 2. Precipitation
# Description: See Metadaten_Parameter_rr_stunde_03379.html in the folder / https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/DESCRIPTION_obsgermany_climate_hourly_precipitation_en.pdf
# Station_id: 03379; München Stadt
# Mess_datum: reference date
# QN_8: quality level
# R1: precipitation
# RS_IND: Precipitation Indicator (yes or no)
# WRTR: ?
precipitation <- read.table(
  "data_folder/weather_data/precipitation/produkt_rr_stunde_19970707_20221231_03379.txt",
  sep = ";", header = TRUE)

# 3. Sun
# Note: Solar isn't measured at 03379 station
# Description: See Metadaten_Parameter_sd_stunde_03379.html in the folder / https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/sun/DESCRIPTION_obsgermany_climate_hourly_sun_en.pdf
# Station_id: 03379; München Stadt
# Mess_datum: reference date
# QN_7: quality level
# SD_SO: Sunshine duration
sun <- read.table(
  "data_folder/weather_data/sun/produkt_sd_stunde_19850101_20221231_03379.txt",
  sep = ";", header = TRUE)

# 4. Wind
# Description: See Metadaten_Parameter_ff_stunde_03379.html in the folder / https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/wind/DESCRIPTION_obsgermany_climate_hourly_wind_en.pdf
# Station_id: 03379; München Stadt
# Mess_datum: reference date
# QN_3: quality level
# F: wind speed
# D: wind direction
wind <- read.table(
  "data_folder/weather_data/wind/produkt_ff_stunde_19850101_20221231_03379.txt",
  sep = ";", header = TRUE
)

colnames(air_temp) <- c("station_id", "date", "quality_level", "air_temp", "humidity", "eor")
colnames(precipitation) <- c("station_id", "date", "quality_level", "precipitation", "precipitation_ind", "precipitation_form", "eor")
colnames(sun) <- c("station_id", "date", "quality_level", "sun_time", "eor")
colnames(wind) <- c("station_id", "date", "quality_level", "wind_speed","wind_direction", "eor")

air_temp <- extract_DateTime(air_temp)
precipitation <- extract_DateTime(precipitation)
sun <- extract_DateTime(sun)
wind <- extract_DateTime(wind)

# Filter years from 2008 to 2022
# to filter out relevant years from hourly recorded weather data
year_filter <- unique(df_cycling$year)
air_temp <- air_temp %>% filter(year %in% year_filter)
precipitation <- precipitation %>% filter(year %in% year_filter)
sun <- sun %>% filter(year %in% year_filter)
wind <- wind %>% filter(year %in% year_filter)

# Remove non-informative columns
# quality_level, air_temp, humidity, precipitation, sun_time, wind_speed, hour, year, month, day will be used for analysis
air_temp <- air_temp %>% select(!c("station_id", "date", "eor"))
precipitation <- precipitation %>% select(c("precipitation", "hour", "year", "month", "day"))
sun <- sun %>% select(!c("station_id", "date", "quality_level", "eor"))
wind <- wind %>% select(c("wind_speed", "hour", "year", "month", "day"))

# NA -999 (placeholder for NA in original set)
precipitation <- precipitation %>% mutate(precipitation = replace(precipitation, which(precipitation == -999), NA))
air_temp <- air_temp %>% mutate(air_temp = replace(air_temp, which(air_temp == -999), NA))
sun <- sun %>% mutate(sun_time = replace(sun_time, which(sun_time == -999), NA))
wind <- wind %>% mutate(wind_speed = replace(wind_speed, which(wind_speed == -999), NA))

# 15 minute data 
precipitation$precipitation <- precipitation$precipitation * 0.25

#--------------#
#----Merging----
#--------------# 
# Merge all the weather data so that there are no NA values
weather_all <- # nrow == 97,255
  merge(air_temp, precipitation, by = c("year", "month", "day", "hour")) %>%
  merge(., wind, by = c("year", "month", "day", "hour")) %>%
  merge(., sun, by = c("year", "month", "day", "hour"))

# Merge precipitation and air temp; # nrow == 131,405
temp_precip <- merge(air_temp, precipitation, by = c("year", "month", "day", "hour"))

# nrow == 2,846,096
df <- merge(df_cycling, school_holidays, by = 'date', all = TRUE)
df <- merge(df, public_holidays, by = 'date', all = TRUE)
df$school_holiday[is.na(df$school_holiday)] <- 0
df$public_holiday[is.na(df$public_holiday)] <- 0

# This is a summary of when direction_1/direction_2 have NA values for each station
df %>%
  group_by(station) %>%
  filter(!is.na(comment)) %>%
  distinct(year, month, comment) %>%
  print(n = Inf)

# FINAL DATA 
# I added all.x = TRUE in order to keep all dates 
df_all <- merge(df, weather_all, by = c("year", "month", "hour", "day"), all.x = TRUE) # nrow == 2,846,096
df_precip <- merge(df, precipitation, by = c("year", "month", "hour", "day"), all.x = TRUE) # nrow == 2,846,096
df_temp_precip <- merge(df, temp_precip, by = c("year", "month", "hour", "day"), all.x = TRUE) # nrow == 2,846,096


#----------------------------------------------------------#
#----test/train data for each station for each direction----
#----------------------------------------------------------#
# Note: 
# 1. Train set: year 2008 ~ 2018 (about 70%) / Test set: year 2019 ~ 2022 (about 30%)
# 2. As discussed, let's consider only air_temperature and precipitation for weather information

summary(df_temp_precip) # needed to be uploaded

# direction_1
df_train_d1 <-  
  df_temp_precip %>% 
  rename(y = "direction_1") %>%
  filter(year < 2019)

df_test_d1 <- 
  df %>% 
  rename(y = "direction_1") %>%
  filter(year >= 2019)

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
  filter(year < 2019)

df_test_d2 <- 
  df_temp_precip %>% 
  rename(y = "direction_2") %>%
  filter(year >= 2019)

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
