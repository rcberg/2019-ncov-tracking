if(!require(pacman)) install.packages("pacman")
pacman::p_load( tidyverse , 
                janitor ,
                lubridate )

# set to your working directory if you want

setwd("D:/Economics/Projects/2019-ncov-tracking")

################################################
###
###  this code will pull data on cases of the
###  2019 novel coronavirus from the john's
###  hopkins university Github repo which is 
###  maintained/updated daily by ryan lau
###
################################################

## confirmed cases

raw_confirmed_data = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")

confirmed_data_df = raw_confirmed_data  %>%
  pivot_longer( -c("Province/State":"Long") , 
                names_to = "date" , 
                values_to = "cases" ) %>%
  mutate( type = "Confirmed Cases" , 
          date = mdy(date) )

## death cases

raw_deaths_data = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")


deaths_data_df = raw_deaths_data  %>%
  pivot_longer( -c("Province/State":"Long") , 
                names_to = "date" , 
                values_to = "cases" ) %>%
  mutate( type = "Death" , 
          date = mdy(date) )

## recovery cases

raw_recov_data = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")


recov_data_df = raw_recov_data  %>%
  pivot_longer( -c("Province/State":"Long") , 
                names_to = "date" , 
                values_to = "cases" ) %>%
  mutate( type = "Recovered" , 
          date = mdy(date) )

## grand dataset 

data_df = confirmed_data_df %>%
  bind_rows( deaths_data_df , 
             recov_data_df ) %>% 
  clean_names() %>%
  mutate(province_state = replace_na(province_state , "Nationwide") ) %>%
  arrange( country_region ,
           desc(date) )

saveRDS( data_df , "data/export/worldwide_data.rds" )

## united states datasets

# us state data

state_data_df =
  data_df %>% filter( country_region == "US" )

saveRDS( state_data_df , "data/export/usa_state_data.rds" )
# us nationwide data

usa_data_df = 
  state_data_df %>% 
  filter( province_state %in% c(state.name, "Washington, D.C." , "Diamond Princess" , "Grand Princess") ) %>%
  group_by( date , type ) %>%
  summarise( cases = sum(cases) ) %>%
  arrange( desc(date)) %>%
  mutate( lat = 38.5266 , 
          long = -96.7265 )

saveRDS( usa_data_df , "data/export/usa_national_data.rds" )
