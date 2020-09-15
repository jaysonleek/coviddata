rm(list = ls())

library(dplyr)

data(airquality)

airquality$Year <- "2000"

airquality$Date <- as.Date(as.character(with(airquality, paste(Year, Month, Day, sep = "-"))))

airquality$Ozone[is.na(airquality$Ozone)] <- 25L

airquality$Solar.R[is.na(airquality$Solar.R)] <- mean(airquality$Solar.R, na.rm = TRUE)

airquality$FIPS <- "25025"

covidcases <- airquality %>% 
  select(Date, FIPS, NewDeaths = Ozone, NewConfirmedCases = Solar.R)

View(covidcases)

covidcases$Yesterday <- NA

covidcases$TotalConf <-
  sapply(seq_len(nrow(covidcases)), 
         function(x) with(covidcases, 
                          sum(NewConfirmedCases[Date <= Date[x]], na.rm = TRUE)))

  
covidcases$TotalConf <-
  sapply(seq_len(nrow(covidcases)), 
         function(x) with(covidcases, 
        sum(NewConfirmedCases[Date <= Date[x]], na.rm = TRUE)))

covidcases$ThreeDayAvg <- 
  sapply(seq_len(nrow(covidcases)),
         function(x) with(covidcases,
                          mean(NewConfirmedCases[Date <= Date[x] - 2], na.rm = TRUE)))

N <- 10

covidcases$NDaysAgo <- 
  sapply(seq_len(nrow(covidcases)),
         function(x) with(covidcases,
          sum(TotalConf[Date == Date[x] - N], na.rm = TRUE)))
         
covidcases$Active <- covidcases$TotalConf - covidcases$NDaysAgo
