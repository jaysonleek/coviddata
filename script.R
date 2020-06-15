## 1. Build the states data.frame----

## Clear the environment

rm(list = ls())

## set working dir


setwd("C:/Users/jayso/OneDrive/Desktop/coviddata")

## call necessary libraries

library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(data.table)

## Read the Data

states <- fread("C:/Users/jayso/OneDrive/Documents/GitHub/covid-19-data/us-states.csv")

## Read the population data

statepop <- fread("C:/RWD/countypopbyfips2.csv")


## merge state data with state population


states <- left_join(states, statepop, by = "fips", all.x = TRUE)




## Make the date column a format that R can recognize

states$date <- as.Date(states$date)

## Add a column for yesterday's cases

states$adayago <- sapply(seq_len(nrow(states)), 
    function(ayer) with(states, sum(cases[date == (date[ayer] - 1) & state == state[ayer]])))

## Add a column for today's new cases

states$newcases <- states$cases - states$adayago

## 7 day average of new cases

states$sevdayavg <- sapply(seq_len(nrow(states)), 
                         function(ayer) with(states, mean(newcases[date >= (date[ayer] - 7) & date <= date[ayer] & state == state[ayer]])))

## 7 day average 7 days ago

states$sevdayavgwkago <- sapply(seq_len(nrow(states)), 
                           function(ayer) with(states, sum(sevdayavg[date == (date[ayer] - 7) & state == state[ayer]])))

## Change in 7 day average since 7 days ago

states$sevdayavgchange <- states$sevdayavg - states$sevdayavgwkago


## Add a column for number of cases one week ago

states$aweekago <- sapply(seq_len(nrow(states)), function(semana) with(states, sum(cases[date == (date[semana] - 7) & state == state[semana]])))

## Add a column for this week's new cases

states$weeklynew <- states$cases - states$aweekago

## Add a column for cases 3 weeks ago 

states$weeksago <- sapply(seq_len(nrow(states)), function(act) with(states, sum(cases[date == (date[act] - 21) & state == state[act]])))

## Add a column for the estimated number of active cases (total - total from 3 weeks ago)

states$active <- states$cases - states$weeksago

## Add a column to calculate the active cases per 100,000 people

states$activepp <- states$active / states$Population * 100000

## Add a column to calculate the number of recovered cases (total - active - deaths)

states$recovered <- states$cases - states$active - states$deaths

## Add a column to calculate the number of recovered cases per 100,000 people

states$recoveredpp <- states$recovered / states$Population * 100000

## Add a column to calculate weekly growth as a percent
states$weeklygrowth <- states$weeklynew / states$aweekago

## Add a column to calculate weekly growth per 100,000 people

states$actgrowthpp <- states$active - 

## Add a column for total cases per 100,000

states$casespp <- states$cases / states$Population * 100000

## turn states into factor

states$state <-as.factor(states$state)

## Load purrr

library(purrr)

## Create a 7 day rolling average for new cases

states$newcasesavg <- sapply(seq_len(nrow(states)), function(act) with(states, mean(newcases[date <= (date[act] - 7) & state == state[act]], na.rm = TRUE)))


## 2. Functions for analyzing and plotting the states data.frame ----

## Load ggplot2

library(ggplot2)

## Function to plot a state's total and active per 100,000 curves

plotpp <- function(st) { # st = the state whose data we wish to plot
  s1 <- subset(states, state == st)
  t1 <- deparse(substitute(st))
  t2 <- paste(t1, "Per 100k People")
    ggplot(s1, aes(x = date)) +
    geom_line(aes(y = activepp), color = "blue", size = 2) +
    geom_line(aes(y = casespp), color = "black", size = 2) +
    labs(title = t2, y = "Total / Active", x = "Date")
  }

## Function to compare 3 states activepp curves

plot3active <- function(st1, st2, st3) { # st = the state whose data we wish to plot
  s1 <- subset(states, state == st1 | state == st2 | state == st3)
  t1 <- deparse(substitute(st1))
  t2 <- deparse(substitute(st2))
  t3 <- deparse(substitute(st3))
  t4 <- paste("Active per 100K")
  ggplot(s1, aes(x = date)) +
    facet_wrap(~state) +
    geom_line(aes(y = activepp), color = "blue", size = 2) +
    labs(title = t4, x = "Date")
}


plot37DA <- function(st1, st2, st3) { # st = the state whose data we wish to plot
  s1 <- subset(states, state == st1 | state == st2 | state == st3)
  t1 <- deparse(substitute(st1))
  t2 <- deparse(substitute(st2))
  t3 <- deparse(substitute(st3))
  t4 <- paste("7 Day Avg of New Cases")
  ggplot(s1, aes(x = date)) +
    facet_wrap(~state) +
    geom_line(aes(y = sevdayavg), color = "blue", size = 2) +
    labs(title = t4, x = "Date")
}


## Filter the latest numbers

date1 <- max(states$date)
statestoday <- filter(states, states$date == date1)
fastestgrowth <- statestoday %>% 
  select(state, weeklygrowthpp) %>% 
  arrange(desc(weeklygrowthpp))




library(ggplot2)
ggplot(states2$Oklahoma, aes(x = date)) +
  geom_line(aes(y = activepp), color = "blue", size = 2) +
  geom_line(aes(y = casespp), color = "black", size = 2) +
  geom_line(aes(y = recoveredpp), color = "red", size = 2)


## 3. Load the counties data ----

## **Don't try and load the counties data and the states data at the same time**

# Clear the environment to make some space - this file contains many observations

rm(list = ls())

## set working dir

setwd("C:/Users/jayso/OneDrive/Desktop/coviddata")

## call required libraries

library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(data.table)

# Read the counties data

counties <- fread("C:/Users/jayso/OneDrive/Documents/GitHub/covid-19-data/us-counties.csv")

## NYC/KC/territory fix

counties$fips[counties$county == "New York City"] = 361111
counties$fips[counties$county == "Kansas City"] = 291111
counties$fips[counties$state == "Puerto Rico"] = 7200
counties$fips[counties$state == "Guam"] = 6600
counties$fips[counties$state == "Virgin Islands"] = 7800
counties$fips[counties$state == "Northern Mariana Islands"] = 6900



## Read county population values

countypop <- fread("C:/RWD/countypopbyfips2.csv")
cbsa <- fread("C:/Users/jayso/OneDrive/Desktop/coviddata/fipscsacbsa.csv")

## merge county data with county population


counties <- left_join(counties, countypop, by = "fips", all.x = TRUE)
counties <- left_join(counties, cbsa, by = "fips", all.x = TRUE)



## Make the date column a format that R will recognize

counties$date <- as.Date(counties$date)

## Make fips, state, county factors

counties$fips <- as.factor(counties$fips)
counties$state <- as.factor(counties$state)
counties$county <- as.factor(counties$county)

## Add a column for yesterday's cases (with timer so I have a benchmark for judging improvements)

system.time({
        counties$adayago <-
                sapply(seq_len(nrow(counties)), 
                function(x) with(counties, 
                sum(cases[date == (date[x] - 1) & fips == fips[x]], na.rm = TRUE)))

  counties$aweekago <-
                sapply(seq_len(nrow(counties)), 
                function(x) with(counties, 
                sum(cases[date == (date[x] - 7) & fips == fips[x]], na.rm = TRUE)))

  counties$thrweekago <-
                sapply(seq_len(nrow(counties)), 
                function(x) with(counties, 
                sum(cases[date == (date[x] - 21) & fips == fips[x]], na.rm = TRUE)))})

  counties$active <- counties$cases - counties$thrweekago
  

  counties$newcases <- counties$cases - counties$adayago
  
  counties$svnDAnc <-
    sapply(seq_len(nrow(counties)), 
           function(x) with(counties, 
                            mean(newcases[date >= (date[x] - 7) & date <= date[x] & fips == fips[x]], na.rm = TRUE)))
  
    counties$svnDAlast <- 
    sapply(seq_len(nrow(counties)),
            function(sumx) with(counties,
            sum(svnDAnc[date == (date[sumx] - 7) & fips == fips[sumx]], na.rm = TRUE)))
  
  
    system.time({
    counties$actadayago <-
                sapply(seq_len(nrow(counties)), 
                function(x) with(counties, 
                sum(active[date == (date[x] - 1) & fips == fips[x]], na.rm = TRUE)))

  counties$actaweekago <-
                sapply(seq_len(nrow(counties)), 
                function(x) with(counties, 
                sum(active[date == (date[x] - 7) & fips == fips[x]], na.rm = TRUE)))
  
counties$actgrowthpp <- (counties$active - counties$actaweekago) / counties$population *100000
})




  
  
  
countiesDT$newcases <- ifelse(countiesDT$cases - countiesDT$adayago < 0, 0, countiesDT$cases - countiesDT$adayago)

countiesDT$newcasesW <- ifelse(countiesDT$cases - countiesDT$aweekago < 0, 0, countiesDT$cases - countiesDT$aweekago)

countiesDT$active <- ifelse(countiesDT$cases - countiesDT$thrweekago < 0, 0, countiesDT$cases - countiesDT$thrweekago)


NYCfix <- rbind(c("New York City", "New York", "361111"))
colnames(NYCfix) <- c("county", "state", "fips")
countiesDT <- left_join(countiesDT, NYCfix2, by = "fips", all.x = TRUE)
NYCfix2$fips <- as.integer(NYCfix2$fips)


system.time({
        RI$adayago <- sapply(seq_len(nrow(RI)), 
               function(ayer) with(RI, 
               sum(cases[date == (date[ayer] - 1) & fips == fips[ayer]], na.rm = TRUE)))})

system.time({
          RI$adayago <- sum(RI$cases[date == (RI$date - 1) & fips == RI$fips])
})


data.frame(Count = colSums(df[,-1] == 'Y'),    # count of "Y"s in each column
           # sum of Values column where A/B/C is "Y"
           Sum = sapply(df[,-1], function(x){sum(df$Values[x == 'Y'])}))


RI$adayago <- sapply(seq_len(nrow(RI)),
              function(x) with(RI, 
              sum(cases[date == (date[x] - 1) & fips == fips[x]], na.rm = TRUE)))



#### FYI - system.time call - user, 519, system, 109, elapsed 630.33

countiesDT$newcases <-  countiesDT$cases - countiesDT$adayago

### A week ago - with system.time call

system.time({
  countiesDT$aweekago <- sapply(seq_len(nrow(countiesDT)), 
             function(ayer) with(countiesDT, 
             sum(cases[date == (date[ayer] - 7) & fips == fips[ayer]])))})

## user  system elapsed 
## 526.89  101.89  631.96 


system.time({
  TX$aweekago <- sapply(seq_len(nrow(TX)), 
        function(x) with(TX, 
        sum(cases[date == (date[x] - 7) & fips == fips[x]])))})

system.time({
  TX$thrwkago <- sapply(seq_len(nrow(TX)), 
                        function(x) with(TX, 
                                         sum(cases[date == (date[x] - 21) & fips == fips[x]])))})

TX$actthrwkago <- sapply(seq_len(nrow(TX)),
                         function(x) with(TX, 
                                          sum(active[date == (date[x] - 21) & fips == fips[x]])))



system.time({
  RI$aweekago <- lapply(seq_len(nrow(RI)), 
                       function(ayer) with(RI, 
                                           sum(cases[date == (date[ayer] - 7) & fips == fips[ayer]], na.rm = TRUE)))})

## group by city

basic_summ = filter(counties, date == as.Date("2020-06-13"))
basic_summ = group_by(basic_summ, cbsatitle, csatitle)
basic_summ = summarise(basic_summ, sumcases = sum(cases), sumactive = sum(active), 
                       sumpop = sum(population), activepp = sum(active) / sum(population) * 100000)

## group by state
state_summ = filter(counties, date == as.Date("2020-06-13"))
state_summ = group_by(state_summ, state)
state_summ = summarise(state_summ, sumcases = sum(cases, na.rm = T), sumactive = sum(active, na.rm = T), 
                       sumpop = sum(population, na.rm = T), activepp = sum(active, na.rm = T) / sum(population, na.rm = T) * 100000)

state_summ <- state_summ[state_summ$sumpop > 0,]

str(countiesDT)


## group by division

div_summ = filter(counties, date == as.Date("2020-05-13"))
div_summ = group_by(div_summ, division)
div_summ = summarise(div_summ, sumcases = sum(cases, na.rm = T), sumactive = sum(active, na.rm = T),
                     sumpop = sum(population, na.rm = T), 
                     activepp = sum(active, na.rm = T) / sum(population, na.rm = T) * 100000)


## filter by state

pkstate <- function(pk) {
        stname <- deparse(substitute(pk))
        stname2 <- noquote(stname)
        sta1 <- filter(counties, state == pk)
        assign(stname2, sta1, env=.GlobalEnv)
        }


getwd()


## Filter the latest numbers

date1 <- max(TX$date)
countiestoday <- filter(TX, TX$date == date1)
fastestgrowth <- countiestoday %>% 
  select(county, activegrowth) %>% 
  arrange(desc(activegrowth))





## 4. Plot some county stuff ----

plot3county <- function(co1, co2, co3) { # co = the counties whose data we wish to plot
  s1 <- subset(counties, county == co1 | county == co2 | county == co3)
  t1 <- deparse(substitute(co1))
  t2 <- deparse(substitute(co2))
  t3 <- deparse(substitute(co3))
  t4 <- paste("Active per 100K")
  ggplot(s1, aes(x = date)) +
    facet_wrap(~county) +
    geom_line(aes(y = activepp), color = "blue", size = 2) +
    labs(title = t4, x = "Date")
}  


plot3fips <- function(f1, f2, f3) { # co = the counties whose data we wish to plot
  s1 <- subset(counties, fips == f1 | fips == f2 | fips == f3)
  t4 <- paste("Active per 100K")
  ggplot(s1, aes(x = date)) +
    facet_wrap(~county) +
    geom_line(aes(y = activepp), color = "blue", size = 2) +
    labs(title = t4, x = "Date")
}  