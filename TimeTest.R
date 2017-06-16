# Timestamp testing
setwd('./DIFAR Testing/')
library(stringr)
library(RSQLite)
library(dplyr)
library(lubridate)

con <-dbConnect(drv=SQLite(), 'TimeTest.sqlite3')
timedf <- dbReadTable(con, 'DIFAR_Localisation') %>% mutate(
      Buoy = factor(Channel,levels=c(0,1,2,3), labels=c('NE', 'SW', 'SE', 'NW')),
      TriggerName = str_trim(TriggerName),
      Species = str_trim(Species),
      MatchedAngles = str_trim(MatchedAngles),
      TrackedGroup = str_trim(TrackedGroup),
      posixDate = ymd_hms(UTC))
dbDisconnect(con)
timedf <- filter(timedf, Id > 24)
timedf <- mutate(timedf,
                 TimeDiff = as.numeric(difftime(UTC, timedf$UTC[1], units='secs')))