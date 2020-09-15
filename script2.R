## exploring data.table

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


statesDT <- data.table(states)






statesDT[, newcases := {tmp <- sapply(seq_len(nrow(statesDT)),
                      function(x) with(statesDT, 
                      sum(cases[date == (date[x] - 1) & state == state[x]]))); cases - tmp}]


statesDT[, activepp := {tmp <- sapply(seq_len(nrow(statesDT)),
                        function(x) with(statesDT, 
                        sum(cases[date == (date[x] - 21) & state == state[x]]))); 
                        (cases - tmp) / population * 100000}]


todayST <- filter(statesDT, date == max(statesDT$date))


todayST[, actppvnatavg := {tmp <- sum(cases) / sum(population * 100000); activepp / tmp}]


statesDT[, peaked := ]

statesDT[, active := {tmp <- sapply(seq_len(nrow(statesDT)),
                      function(x) with(statesDT,
                      sum(cases[date == (date[x] - 21) & fips == fips[x]]))); cases - tmp}]




statesDT[, actppvnatavg := {tmp <- sapply(seq_len(nrow(statesDT)),
                      function(x) with(statesDT, 
                      mean(activepp[date <= (date[x])]))); 
                      activepp / tmp}]


todayST <- filter(statesDT, date == max(date))

todayST[, natavg := sum(active) / sum(population) * 100000]

natavg <- sum(todayST$active / sum(todayST$population) * 100000)

todayST$actppvnat <- todayST$activepp / natavg


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

California$weeksago <- sapply(seq_len(nrow(California)), function(act) with(California, sum(cases[date == (date[act] - 21)])))


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


