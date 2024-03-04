#Packages ----
#----------------------------#
library(dplyr)
library(lubridate)
library(stringr)
library(mgcv)
library(ggplot2)
library(car)
library(RColorBrewer)
library(hrbrthemes)
library(plotly)
library(htmlwidgets)

#Show yearly seasonal cycle for each station, Clear Seasonality for all stations direction_1-----
#ALL
df_cycling %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_1) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("ALL: Mean cyclists for each day across the years, direction_1") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))
#Arnulf
df_cycling %>% 
  filter(station == "Arnulf") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_1) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Arnulf: Mean cyclists for each day across the years, direction_1") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))
#Erhardt
df_cycling %>% 
  filter(station == "Erhardt") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_1) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Erhardt: Mean cyclists for each day across the years, direction_1") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Olympia
df_cycling %>% 
  filter(station == "Olympia") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_1) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Olympia: Mean cyclists for each day across the years, direction_1") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Kreuther
df_cycling %>% 
  filter(station == "Kreuther") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_1) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Kreuther: Mean cyclists for each day across the years, direction_1") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Margareten
df_cycling %>% 
  filter(station == "Margareten") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_1) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Margareten: Mean cyclists for each day across the years, direction_1") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Hirsch
df_cycling %>% 
  filter(station == "Hirsch") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_1) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Arnulf: Mean cyclists for each day across the years, direction_1") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Show yearly seasonal cycle for each station, Clear Seasonality for all stations direction_2-----
#ALL
df_cycling %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_2) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("ALL: Mean cyclists for each day across the years, direction_2") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))
#Arnulf
df_cycling %>% 
  filter(station == "Arnulf") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_2) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Arnulf: Mean cyclists for each day across the years, direction_2") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Erhardt
df_cycling %>% 
  filter(station == "Erhardt") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_2) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Erhardt: Mean cyclists for each day across the years, direction_2") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Olympia
df_cycling %>% 
  filter(station == "Olympia") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_2) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Arnulf: Mean cyclists for each day across the years, direction_2") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Kreuther
df_cycling %>% 
  filter(station == "Kreuther") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_2) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Arnulf: Mean cyclists for each day across the years, direction_2") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Margareten
df_cycling %>% 
  filter(station == "Margareten") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_2) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Arnulf: Mean cyclists for each day across the years, direction_2") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))

#Hirsch
df_cycling %>% 
  filter(station == "Hirsch") %>% 
  mutate(date_dm = format(date, format="%m-%d")) %>% 
  select(date_dm, direction_2) %>% 
  group_by(date_dm) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T))  %>% 
  mutate(date_dm = as.Date(date_dm, format="%m-%d")) %>% 
  ggplot(aes(x= date_dm, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("Arnulf: Mean cyclists for each day across the years, direction_2") +
  xlab("day")+
  scale_x_date(date_breaks = "months" , date_labels = "%b",expand = c(0, 0))



#Show daily and sub-daily seasonality, cyclists per day of week and hour of the day (heatmap)----
#Arnulf
df_cycling %>%     
  filter(station == "Arnulf") %>% 
  select(weekday, direction_1, hour) %>% 
  group_by(hour,weekday) %>% 
  summarise(mean_cyclists = mean(direction_1, na.rm=T))  %>% 
  ggplot(aes(y= weekday, x= hour, fill = mean_cyclists)) +
  geom_tile() +
  ggtitle("Arnulf: number of cyclists per day of week and hour of day") +
  scale_fill_gradient(low="white", high="black")+
  xlab("hour of the day") +
  ylab("day of the week") 

pp <- ggplotly(tooltip="all")
saveWidget(pp, file=paste0( getwd(), "/HtmlWidget/ArnulfHeat.html"))

#sub daily: looking at week days versus week-ends, direction_1----
#Split set into weekday and weekend
df_mean_cyclists_per_day_weekday <- df_cycling %>%     
  filter(weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) %>% 
  select(weekday, direction_1, direction_2, hour, station) %>% 
  group_by(hour, station) %>% 
  summarise(mean_cyclists_d1 = mean(direction_1, na.rm=T), mean_cyclists_d2 = mean(direction_2, na.rm=T))


df_mean_cyclists_per_day_weekend <- df_cycling %>% 
  filter(weekday %in% c("Saturday", "Sunday")) %>% 
  select(weekday, direction_1,direction_2, hour, station) %>% 
  group_by(hour, station) %>% 
  summarise(mean_cyclists_d1 = mean(direction_1, na.rm=T), mean_cyclists_d2 = mean(direction_2, na.rm=T))

#ALL
ggplot() +
  df_mean_cyclists_per_day_weekday %>% group_by(hour) %>% summarise(mean_cyclists = mean(mean_cyclists_d1)) %>% geom_line(mapping =aes(x= hour, y = mean_cyclists, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% group_by(hour) %>% summarise(mean_cyclists = mean(mean_cyclists_d1)) %>% geom_line(mapping = aes(x= hour, y = mean_cyclists, color = "weekend"))+
  ggtitle("ALL: number of cyclists per hour of the day, direction_1") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Arnulf
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Arnulf") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d1, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Arnulf") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d1, color = "weekend"))+
  ggtitle("Arnulf: number of cyclists per hour of the day, direction_1") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Erhardt
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Erhardt") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d1, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Erhardt") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d1, color = "weekend"))+
  ggtitle("Erhardt: number of cyclists per hour of the day, direction_1") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Olympia
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Olympia") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d1, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Olympia") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d1, color = "weekend"))+
  ggtitle("Olympia: number of cyclists per hour of the day, direction_1") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Kreuther
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Kreuther") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d1, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Kreuther") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d1, color = "weekend"))+
  ggtitle("Kreuther: number of cyclists per hour of the day, direction_1") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Margareten
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Margareten") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d1, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Margareten") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d1, color = "weekend"))+
  ggtitle("Margareten: number of cyclists per hour of the day, direction_1") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Hirsch
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Hirsch") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d1, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Hirsch") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d1, color = "weekend"))+
  ggtitle("Hirsch: number of cyclists per hour of the day, direction_1") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))



#sub daily: looking at week days versus week-ends, direction_2-----
#ALL
ggplot() +
  df_mean_cyclists_per_day_weekday %>% group_by(hour) %>% summarise(mean_cyclists = mean(mean_cyclists_d2)) %>% geom_line(mapping =aes(x= hour, y = mean_cyclists, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% group_by(hour) %>% summarise(mean_cyclists = mean(mean_cyclists_d2)) %>% geom_line(mapping = aes(x= hour, y = mean_cyclists, color = "weekend"))+
  ggtitle("ALL: number of cyclists per hour of the day, direction_2") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Arnulf
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Arnulf") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d2, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Arnulf") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d2, color = "weekend"))+
  ggtitle("Arnulf: number of cyclists per hour of the day, direction_2") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Erhardt
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Erhardt") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d2, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Erhardt") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d2, color = "weekend"))+
  ggtitle("Erhardt: number of cyclists per hour of the day, direction_2") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Olympia
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Olympia") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d2, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Olympia") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d2, color = "weekend"))+
  ggtitle("Olympia: number of cyclists per hour of the day, direction_2") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Kreuther
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Kreuther") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d2, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Kreuther") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d2, color = "weekend"))+
  ggtitle("Kreuther: number of cyclists per hour of the day, direction_2") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Margareten
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Margareten") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d2, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Margareten") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d2, color = "weekend"))+
  ggtitle("Margareten: number of cyclists per hour of the day, direction_2") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))

#Hirsch
ggplot() +
  df_mean_cyclists_per_day_weekday %>% filter(station =="Hirsch") %>% geom_line(mapping =aes(x= hour, y = mean_cyclists_d2, color = "weekday"))+
  df_mean_cyclists_per_day_weekend %>% filter(station =="Hirsch") %>% geom_line(mapping = aes(x= hour, y = mean_cyclists_d2, color = "weekend"))+
  ggtitle("Hirsch: number of cyclists per hour of the day, direction_2") +
  xlab("hour of the day") +
  ylab("cyclists") +
  scale_color_manual(name = "", 
                     values = c("weekday" = "blue", "weekend" = "red")) +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0))






#Full time series, show trend, mean cyclists direction _1----
#ALL
df_cycling %>% 
  group_by(date) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T))  %>% 
  ggplot(aes(x= date, y= mean_cyclists_per_day)) +
  geom_line() +
  ggtitle("ALL: Time series, direction_2") +
  xlab("Date") +
  theme_light() +
  scale_x_date(date_breaks = "years" , date_labels = "%Y",expand = c(0, 0))

#Show yearly trend direction_1 and 2 ----
#ALL
ggplot() +
  df_cycling %>% 
  group_by(year, station) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T)) %>% 
  geom_line(mapping = aes(x = year, y= mean_cyclists_per_day, color = station)) +
  df_cycling %>% 
  group_by(year) %>% 
  summarise(mean_cyclists_per_day = mean(direction_1, na.rm=T)) %>% 
  geom_line(mapping = aes(x = year, y= mean_cyclists_per_day, color = "All stations")) +
  ggtitle("ALL: Yearly averaged cyclists, direction_1") +
  xlab("Date") +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0)) 

ggplot() +
  df_cycling %>% 
  group_by(year, station) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T)) %>% 
  geom_line(mapping = aes(x = year, y= mean_cyclists_per_day, color = station)) +
  df_cycling %>% 
  group_by(year) %>% 
  summarise(mean_cyclists_per_day = mean(direction_2, na.rm=T)) %>% 
  geom_line(mapping = aes(x = year, y= mean_cyclists_per_day, color = "All stations")) +
  ggtitle("ALL: Yearly averaged cyclists, direction_2") +
  xlab("Date") +
  theme_light() +
  scale_x_continuous(breaks = scales::breaks_width(1),expand = c(0, 0)) 


