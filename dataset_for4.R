################################################################################
# Datasets for Data Story 4: Sewanee utilities & weather
################################################################################

# ******************************************************************************
# Ensure "sewanee_weather.rds" & "utilities.rds" are in your working directory
# ******************************************************************************

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)

rm(list = ls()) # clear environment first
dir() # look at files in your working directory

# weather ======================================================================
load('sewanee_weather.rds') # loads 3 datasets

# dataset #1: Monthly rainfall in Sewanee, 1895 - 2023
sewanee_rain %>% head
sewanee_rain %>% tail

# dataset #2: Monthly temperature in Sewanee, 1958 - 2023
# Note some years have wonky data
sewanee_temp$year %>% unique
# So let's take those rows out
sewanee_temp <- sewanee_temp %>% filter(!is.na(as.numeric(year)))
# Now take a look
sewanee_temp %>% head
sewanee_temp %>% tail

# dataset #3: Hourly weather (air temp, soil temp, humidity, rain) from Split Creek Observatory
# Aug 18, 2018 - June 14 2022
split_creek %>% head
split_creek %>% tail

# utilities  ===================================================================
load('utilities.rds') # loads two datasets

# dataset #1: Utilities data for every campus building (water, electricity, natural gas)
# caution: many rows have missing data
utilities %>% as.data.frame %>% head
utilities %>% as.data.frame %>% tail

# dataset #2: Same data for Fall 2025, but with residence hall occupancy information added
# broken down by gender
# caution again: many rows have missing data
fall2025 %>% as.data.frame %>% head
fall2025 %>% as.data.frame %>% tail

################################################################################

ggplot(sewanee_rain,
       aes(x = year,
           y = inches,
           colour = month)) +
  geom_point()

ggplot(sewanee_temp %>% filter(year == 2000:2023),
       aes(x = year,
           y = temp,
           group = year)) +
  geom_boxplot()

# rain plot ----
sewanee_rain <- 
  sewanee_rain %>%
  mutate(time = make_date(year = sewanee_rain$year,
                          month = 1,
                          day = 1))
## by month ----
sewanee_rain_month <- 
  sewanee_rain %>% 
  mutate(time = paste(month, year)) %>%
  mutate(time = my(time))

ggplot(sewanee_rain_month,
       aes(x = time,
           y = inches))+
  geom_area(fill = "blue")

## by season ----
spring <- c("March","April","May")
summer <- c("June","July","August")
autumn <- c("September","October","November")
winter <- c("December","January","February")

sewanee_rain_season <- 
  sewanee_rain %>%
  mutate(season = month)

for (i in spring){
  sewanee_rain_season <-
    sewanee_rain_season %>%
    mutate(season = gsub(i, "spring", sewanee_rain_season$season))
  }

for (i in summer){
  sewanee_rain_season <-
    sewanee_rain_season %>%
    mutate(season = gsub(i, "summer", sewanee_rain_season$season))
}

for (i in autumn){
  sewanee_rain_season <-
    sewanee_rain_season %>%
    mutate(season = gsub(i, "autumn", sewanee_rain_season$season))
}

for (i in winter){
  sewanee_rain_season <-
    sewanee_rain_season %>%
    mutate(season = gsub(i, "winter", sewanee_rain_season$season))
}

sewanee_rain_season <-
  sewanee_rain_season %>%
  group_by(year, season) %>%
  summarise(total_inches = sum(inches, na.rm = TRUE)) %>%
  mutate(ID = row_number()) 

sewanee_rain_season$ID <- gsub("1", "09", sewanee_rain_season$ID)
sewanee_rain_season$ID <- gsub("2", "03", sewanee_rain_season$ID)
sewanee_rain_season$ID <- gsub("1", "06", sewanee_rain_season$ID)
sewanee_rain_season$ID <- gsub("1", "11", sewanee_rain_season$ID)

sewanee_rain_season <- 
  sewanee_rain_season %>% 
  mutate(time = paste(ID, year)) %>%
  mutate(time = my(time))

ggplot(sewanee_rain_season,
       aes(x = time,
           y = total_inches)) +
  geom_area()

## by year ----
sewanee_rain_year <-
  sewanee_rain %>% 
  group_by(time) %>%
  summarise(total_inches = sum(inches, na.rm = TRUE))

ggplot(sewanee_rain_year,
       aes(x = time,
           y = total_inches)) +
  geom_area(fill = 'blue')



# temperature plot ----

## basic ----
sewanee_temp <- 
  sewanee_temp %>%
  mutate(time = paste(month, year),
         temp = as.numeric(temp)) %>%
  mutate(time = my(time))
  
ggplot(sewanee_temp,
       aes(x = time,
           y = temp,
           color = stat)) +
  geom_path()

## for each month ----
temp_month_plot <- 
  function(m){
    sewanee_temp_month <- 
      sewanee_temp %>%
      filter(month == m) 
    
    ggplot(sewanee_temp_month,
           aes(x = time,
               y = temp,
               color = stat)) +
      geom_path()
  }

temp_month_plot(m = "January")

## for each year ----

