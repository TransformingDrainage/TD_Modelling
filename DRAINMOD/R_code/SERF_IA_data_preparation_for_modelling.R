library(readr)
library(stringr)
library(lubridate)
library(dplyr)

# TILE FLOW =======================================================================================

serfia_flow <- read_csv("~/TD/Research Data/SERF_IA/SERF_Drainage_Data_Flow_HOURLY.csv", 
                        col_types = cols(`S1 WAT1` = col_double(), 
                                         `S2 WAT1` = col_double(), 
                                         `S3 WAT1` = col_double(), 
                                         `S4 WAT1` = col_double(), 
                                         `S5 WAT1` = col_double(), 
                                         `S6 WAT1` = col_double(), 
                                         date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

serfia_flow %>%
  gather(key = plotid, value = WAT1, `S1 WAT1`:`S6 WAT1`) %>%
  mutate(Date = as.Date(date),
         plotid = word(plotid, 1)) %>%
  group_by(Date, plotid) %>%
  summarise(WAT1 = sum(WAT1, na.rm = TRUE)) %>%       # SUM up daily flow
  mutate(WAT1 = WAT1/10) %>%                          # convert from mm to cm
  ungroup() -> serfia_flow_DAILY


# calculate DAILY Tile Flow 
serfia_flow_DAILY %>%
  spread(key = plotid, value = WAT1) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Tile_Flow_cm_daily_SERF_IA.prn",
              sep = "\t", row.names = FALSE, col.names = TRUE)


# calculate MONTHLY Tile Flow 
serfia_flow_DAILY %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month, plotid) %>%
  summarise(WAT1 = sum(WAT1, na.rm = TRUE)) %>%       # SUM up monthly flow
  ungroup() %>%
  spread(key = plotid, value = WAT1) %>%
  arrange(Year, Month) %>%
  mutate(Date = paste(month.abb[Month], Year, sep = "-")) %>%
  select(Date, S1:S6) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Tile_Flow_cm_monthly_SERF_IA.prn",
              sep = "\t", row.names = FALSE, col.names = TRUE)


# calculate YEARLY Tile Flow 
serfia_flow_DAILY %>%
  mutate(Date = year(Date)) %>% 
  group_by(Date, plotid) %>%
  summarise(WAT1 = sum(WAT1, na.rm = TRUE)) %>%
  spread(key = plotid, value = WAT1) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Tile_Flow_cm_yearly_SERF_IA.prn",
              sep = "\t", row.names = FALSE, col.names = TRUE)





# NO3-N LOAD ======================================================================================

serfia_load <- read_csv("~/TD/Research Data/SERF_IA/SERF_Drainage_Data_Load_HOURLY.csv", 
                        col_types = cols(`S1 WAT20` = col_double(), 
                                         `S2 WAT20` = col_double(), 
                                         `S3 WAT20` = col_double(), 
                                         `S4 WAT20` = col_double(), 
                                         `S5 WAT20` = col_double(), 
                                         `S6 WAT20` = col_double(), 
                                         date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

serfia_load %>%
  select(date, matches("WAT20")) %>%
  gather(key = plotid, value = WAT20, `S1 WAT20`:`S6 WAT20`) %>%
  mutate(Date = as.Date(date),
         plotid = word(plotid, 1)) %>%
  group_by(Date, plotid) %>%
  summarise(WAT20 = sum(WAT20, na.rm = TRUE)) %>%       # SUM up daily flow
  ungroup() -> serfia_load_DAILY


# calculate DAILY NO3-N Load
serfia_load_DAILY %>%
  spread(key = plotid, value = WAT20) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/N_Load_daily_SERF_IA.prn",
              sep = "\t", row.names = FALSE, col.names = TRUE)


# calculate MONTHLY NO3-N Load 
serfia_load_DAILY %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  group_by(Year, Month, plotid) %>%
  summarise(WAT20 = sum(WAT20, na.rm = TRUE)) %>%       # SUM up monthly flow
  ungroup() %>%
  spread(key = plotid, value = WAT20) %>%
  arrange(Year, Month) %>%
  mutate(Date = paste(month.abb[Month], Year, sep = "-")) %>%
  select(Date, S1:S6) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/N_Load_monthly_SERF_IA.prn",
              sep = "\t", row.names = FALSE, col.names = TRUE)


# calculate YEARLY NO3-N Load 
serfia_load_DAILY %>%
  mutate(Date = year(Date)) %>% 
  group_by(Date, plotid) %>%
  summarise(WAT20 = sum(WAT20, na.rm = TRUE)) %>%
  spread(key = plotid, value = WAT20) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/N_Load_yearly_SERF_IA.prn",
              sep = "\t", row.names = FALSE, col.names = TRUE)





# WATER TABLE =====================================================================================

serfia_wt <- read_csv("~/TD/Research Data/SERF_IA/SERF_Water_Table_HOURLY.csv", 
                        col_types = cols(`S1 WAT4 Water Table Depth` = col_double(), 
                                         `S2 WAT4 Water Table Depth` = col_double(), 
                                         `S3 WAT4 Water Table Depth` = col_double(), 
                                         `S4 WAT4 Water Table Depth` = col_double(), 
                                         `S5 WAT4 Water Table Depth` = col_double(), 
                                         `S6 WAT4 Water Table Depth` = col_double(),  
                                         `S7 WAT4 Water Table Depth` = col_double(), 
                                         `S8 WAT4 Water Table Depth` = col_double(),
                                         Date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

serfia_wt %>%
  gather(key = plotid, value = WAT4, `S1 WAT4 Water Table Depth`:`S8 WAT4 Water Table Depth`) %>%
  mutate(plotid = word(plotid, 1)) -> serfia_wt_HOURLY


# calculate DAILY AVERAGE Water Table
serfia_wt_HOURLY %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date, plotid) %>%
  summarise(WAT4 = mean(WAT4, na.rm = TRUE)) %>% 
  mutate(WAT4 = round(WAT4, digits = 2)) %>%
  ungroup() %>%
  spread(key = plotid, value = WAT4) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Water_Table_cm_daily_ave_SERF_IA.prn",
              sep = "\t", na =" ", row.names = FALSE, col.names = TRUE)


# calculate MIDNIGHT Water Table or whatever is the first reading after the midnight 
serfia_wt_HOURLY %>%
  filter(!is.na(WAT4)) %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(Date, plotid) %>%
  summarise(WAT4 = first(WAT4)) %>%
  mutate(WAT4 = round(WAT4, digits = 2)) %>%
  ungroup() %>%
  spread(key = plotid, value = WAT4) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Water_Table_cm_midnight_SERF_IA.prn",
              sep = "\t", na =" ", row.names = FALSE, col.names = TRUE)




# WEATHER =========================================================================================

serfia_rain_CR1000 <- read_csv("~/TD/Research Data/SERF_IA/SERF_IA_Precip_CR1000_hourly.csv", 
                               col_types = cols(precip_mm = col_double(),
                                                date = col_datetime(format = "%Y-%m-%d %H:%M:%S")))

serfia_rain_MESONET <- read_csv("~/TD/Research Data/SERF_IA/rain_fall_SERF_IA.txt", 
                                col_types = cols(drct = col_skip(), et = col_skip(), soil04t = col_skip(), 
                                                 speed = col_skip(), station = col_skip(), 
                                                 valid = col_datetime(format = "%Y-%m-%d %H:%M")))

serfia_rain_MESONET %>%
  select(valid, precip) %>%
  mutate(date = force_tz(valid, tzone = "America/Chicago")) %>%
  mutate(date = with_tz(date, tzone = "UTC")- hours(6)) %>%
  mutate(precip_mm =precip * 25.4) %>%
  select(date, precip_mm) -> serfia_rain_2014_2016

serfia_rain_CR1000 %>%
  filter(date < ymd_hms("2014-01-01 00:00:00")) -> serfia_rain_2012_2013


serfia_rain_2012_2013 %>%
  bind_rows(serfia_rain_2014_2016) %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(n = sum(precip_mm > 0), 
            rain = sum(precip_mm)) %>%
  ggplot(aes(x=n, y=rain)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x-1) +
  stat_summary(fun.y = mean, geom="line") +
  labs(x = "Rainfall Duration (hr)",
       y = "Amount of Rainfall (mm)") -> serfia_rain_plot

ggplot_build(serfia_rain_plot)$data[[2]] %>%
  select(x, y) %>%
  ggplot(aes(x, y)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(breaks = seq(0, 48, 4)) +
  geom_hline(yintercept = c(6, 12, 30), colour = "blue") +
  geom_vline(xintercept = c(3, 6, 16), colour = "red", linetype = "dashed")


serfia_rain_FARM <- read_csv("~/TD/Research Data/SERF_IA/SERF_IA_Precip_Farm_daily.csv", 
                               col_types = cols(precip_mm = col_double(),
                                                date = col_datetime(format = "%Y-%m-%d")))

serfia_rain_FARM %>%
  filter(year(date) < 2012) %>%
  filter(!is.na(precip_mm)) %>%
  mutate(duration = ifelse(precip_mm < 6, 2,
                           ifelse(precip_mm < 12, 4,
                                  ifelse(precip_mm < 30, 8, 24)))) %>%
  mutate(start_hr = ifelse(duration == 24, 0, 6)) %>%
  as.data.frame() %>%
  .[rep(row.names(.), .$duration), ] %>%
  mutate(add = rownames(.) %>% 
           gsub(pattern = "\\.", replacement = " ") %>%
           word(start = 2) %>%
           as.numeric()) %>%
  mutate(add = ifelse(is.na(add), 0, add)) %>%
  mutate(date = ymd_h(paste(date, (start_hr + add))),
         precip_mm = precip_mm/duration) %>% 
  select(date, precip_mm) -> serfia_rain_2007_2011

serfia_rain_2007_2011 %>%
  bind_rows(serfia_rain_2012_2013) %>%
  bind_rows(serfia_rain_2014_2016) -> serfia_rain_2007_2016


serfia_snow <- read_csv("~/TD/Research Data/SERF_IA/snowfall_SERF_IA.txt", 
                        trim_ws = FALSE, skip = 4)
  
serfia_snow %>% 
  filter(station_name == "WASHINGTON",
         month(day) %in% c(1, 2, 11, 12)) %>%
  filter(precip > 0) %>%
  mutate(precip_mm = precip * 25.4) %>%
  select(day, precip_mm) %>% 
  filter(!is.na(precip_mm)) %>%
  mutate(duration = ifelse(precip_mm < 6, 2,
                           ifelse(precip_mm < 12, 4,
                                  ifelse(precip_mm < 30, 8, 24)))) %>%
  mutate(start_hr = ifelse(duration == 24, 0, 6)) %>%
  as.data.frame() %>%
  .[rep(row.names(.), .$duration), ] %>%
  mutate(add = rownames(.) %>% 
           gsub(pattern = "\\.", replacement = " ") %>%
           word(start = 2) %>%
           as.numeric()) %>%
  mutate(add = ifelse(is.na(add), 0, add)) %>%
  mutate(date = ymd_h(paste(day, (start_hr + add))),
         precip_mm = precip_mm/duration) %>% 
  select(date, precip_mm) -> serfia_snow_2007_2016
  

serfia_rain_2007_2016 %>% 
  filter(month(date) %in% 3:10) %>% 
  bind_rows(serfia_snow_2007_2016) %>%
  arrange(date) %>%
  filter(precip_mm != 0) -> serfia_precip_mm


serfia_precip_mm %>%
  mutate(year = year(date),
         doy = (yday(date) + hour(date)/24) %>% round(digits =3)) %>%
  select(year, doy, precip_mm) %>% 
  filter(year < 2017) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Rain_mm_hourly_SERF_IA.prn",
              sep = "\t", row.names = FALSE, col.names = FALSE)


serfia_precip_mm %>%
  mutate(date = as.Date(date)) %>%
  group_by(date) %>%
  summarise(precip_mm = sum(precip_mm)) %>%
  mutate(year = year(date),
         doy = yday(date)) %>%
  select(year, doy, precip_mm) %>% 
  filter(year < 2017) %>%
  write.table(file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Rain_mm_daily_SERF_IA.prn",
            sep = "\t", row.names = FALSE, col.names = FALSE)

