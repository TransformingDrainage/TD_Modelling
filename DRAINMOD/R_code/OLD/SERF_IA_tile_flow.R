library(readxl)
library(stringr)
library(lubridate)
library(dplyr)

SERF_IA_Water_Table_Depth <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Water Table Depth.xlsx", 
                                        sheet = "2007", 
                                        skip = 1,
                                        na = "NAN")

names <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Water Table Depth.xlsx", sheet = "2007")
names <- names(names)
names <- word(names)

names(SERF_IA_Water_Table_Depth) <- names
rm(names)

#SERF_IA_Water_Table_Depth$year <- year(SERF_IA_Water_Table_Depth$Date)
SERF_IA_Water_Table_Depth$ymd <- as.Date(SERF_IA_Water_Table_Depth$Date)
SERF_IA_Water_Table_Depth$doy <- yday(SERF_IA_Water_Table_Depth$Date)
SERF_IA_Water_Table_Depth$time <- round.POSIXt(SERF_IA_Water_Table_Depth$Date, units = "mins")
SERF_IA_Water_Table_Depth$time <- format(SERF_IA_Water_Table_Depth$time, "%H:%M")


# calculate daily average WT
SERF_IA_Water_Table_Depth %>% 
  group_by(ymd) %>% 
  summarise(S1 = mean(S1, na.rm = T),
            S2 = mean(S2, na.rm = T),
            S3 = mean(S3, na.rm = T),
            S4 = mean(S4, na.rm = T),
            S5 = mean(S5, na.rm = T),
            S6 = mean(S6, na.rm = T),
            S7 = mean(S7, na.rm = T),
            S8 = mean(S8, na.rm = T)) -> averageWT
# shorter version
SERF_IA_Water_Table_Depth %>% 
  group_by(ymd) %>% 
  summarise_if(is.numeric, mean, na.rm = T) -> averageWT


# extract midnight WT
SERF_IA_Water_Table_Depth %>%
  filter(time == "00:00") -> midnightWT


# extract midnight or interpolate from neighbors 
midrows <- which(SERF_IA_Water_Table_Depth$time == "00:00")

# extract midnight or earlier WT
SERF_IA_Water_Table_Depth %>%
  group_by(ymd) 


head(SERF_IA_Water_Table_Depth, 20)


# TILE FLOW ====================================================

FLOW_1 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2007", 
                     skip = 1,
                     na = "NAN")

FLOW_2 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2008", 
                     skip = 1,
                     na = "NAN")

FLOW_3 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2009", 
                     skip = 1,
                     na = "NAN")

FLOW_4 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2010", 
                     skip = 1,
                     na = "NAN")

FLOW_5 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2011", 
                     skip = 1,
                     na = "NAN")

FLOW_6 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2012", 
                     skip = 1,
                     na = "NaN")

FLOW_7 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2013", 
                     skip = 1,
                     na = "NAN")

FLOW_8 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2014", 
                     skip = 1,
                     na = "NaN")

FLOW_9 <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", 
                     sheet = "2015", 
                     skip = 1,
                     na = "NAN")


#FLOW_mm <- rbind(FLOW_1, FLOW_2, FLOW_3)
FLOW_mm <- do.call(rbind, lapply(ls(pattern = "FLOW"), get))

names <- read_excel("~/Work/TD/MODEL/DRAINMOD data/SERF_IA Tile Flow.xlsx", sheet = "2007")
names <- names(names)
names <- word(names)

names(FLOW_mm) <- names
rm(names)

FLOW_mm$Time <- format(FLOW_mm$Time, "%H:%M")
FLOW_mm$Date <- as.Date(FLOW_mm$Date)
FLOW_mm$doy <- yday(FLOW_mm$Date)

FLOW_mm$S1 <- as.numeric(FLOW_mm$S1)
FLOW_mm$S2 <- as.numeric(FLOW_mm$S2)
FLOW_mm$S3 <- as.numeric(FLOW_mm$S3)
FLOW_mm$S4 <- as.numeric(FLOW_mm$S4)
FLOW_mm$S5 <- as.numeric(FLOW_mm$S5)
FLOW_mm$S6 <- as.numeric(FLOW_mm$S6)

# convert mm to cm
FLOW_cm <- FLOW_mm[c("Date", "Time")]
FLOW_cm <- cbind(FLOW_cm, FLOW_mm[ , matches("S", vars = names(FLOW_mm))] / 10)


# calculate daily flow
FLOW_cm %>% 
  group_by(Date) %>% 
  summarise_at(vars(matches("S")), sum, na.rm = T) -> FLOW_daily
FLOW_daily[ , 2:7] <- round(FLOW_daily[ , 2:7], digits = 3)

# calculate monthy flow 
FLOW_cm %>%
  group_by(year(Date), month(Date)) %>%
  summarise_at(vars(matches("S")), sum, na.rm = T) -> FLOW_monthly
FLOW_monthly$Date <- paste(month.abb[FLOW_monthly$`month(Date)`], FLOW_monthly$`year(Date)`, sep = "-")
FLOW_monthly <- FLOW_monthly[ , c(9, 3:8)]
FLOW_monthly[ , 2:7] <- round(FLOW_monthly[ , 2:7], digits = 3)

# calculate yearly flow
FLOW_cm %>%
  group_by(year(Date)) %>% 
  summarise_at(vars(matches("S")), sum, na.rm = T) -> FLOW_yearly
FLOW_yearly[ , 2:7] <- round(FLOW_yearly[ , 2:7], digits = 3)


write.table(FLOW_daily, file = "C:/Users/Gio/Documents/Work/TD/MODEL/DRAINMOD data/Tile_Flow_cm_daily_SERF.IA.prn",
            sep = '\t', 
            row.names = FALSE,
            na = " ")

write.table(FLOW_monthly, file = "C:/Users/Gio/Documents/Work/TD/MODEL/DRAINMOD data/Tile_Flow_cm_monthly_SERF.IA.prn",
            sep = '\t', 
            row.names = FALSE,
            na = " ")

write.table(FLOW_yearly, file = "C:/Users/Gio/Documents/Work/TD/MODEL/DRAINMOD data/Tile_Flow_cm_yearly_SERF.IA.prn",
            sep = '\t', 
            row.names = FALSE,
            na = " ")

# MISSING VALUES ==============================================

# count missing hourly values on daily base (count)
FLOW_cm %>% 
  group_by(Date) %>%
  summarise_each(funs(if(is.numeric(.)) sum(is.na(.)) else first(.))) %>%
  select(1, 3:8) -> FLOW_daily_missing_count
# count missing hourly values on daily base (percent)
FLOW_cm %>% 
  group_by(Date) %>%
  summarise_each(funs(if(is.numeric(.)) round(sum(is.na(.))/(sum(is.na(.) + !is.na(.)))*100, 1) else first(.))) %>%
  select(1, 3:8) -> FLOW_daily_missing_percent

FLOW_cm %>% 
  group_by(Date) %>%
  summarise_each(funs(if(is.numeric(.)) sum(is.na(.) + !is.na(.)) else first(.))) %>% 
  select(Date, S1) %>% 
  filter(S1 != 48) %>%
  arrange(S1)


write.table(FLOW_daily_missing_count, 
            file = "C:/Users/Gio/Documents/Work/TD/MODEL/DRAINMOD data/Tile_Flow_daily_missing_SERF.IA.prn",
            sep = '\t', 
            row.names = FALSE,
            na = " ")

write.table(FLOW_daily_missing_percent, 
            file = "C:/Users/Gio/Documents/Work/TD/MODEL/DRAINMOD data/Tile_Flow_daily_missing_percent_SERF.IA.prn",
            sep = '\t', 
            row.names = FALSE,
            na = " ")
