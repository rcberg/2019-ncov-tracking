library(tidyverse)
library(janitor)

# code here is to update the CSSE data after some of the previous raw data had spikes and such properly re-allocated

new_covid_case_numbers <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

new_covid_death_numbers <- 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

new_covid_case_numbers <- 
  new_covid_case_numbers %>% 
  pivot_longer( cols = -c(UID,
                         iso2,
                         iso3,
                         code3,
                         FIPS,
                         Admin2,
                         Province_State,
                         Country_Region,
                         Lat,
                         Long_,
                         Combined_Key) , 
                names_to = "date" , 
                values_to = "confirmed" ) %>%
  mutate( date = lubridate::mdy(date)) %>%
  clean_names()

new_covid_death_numbers <- 
  new_covid_death_numbers %>% 
  pivot_longer( cols = -c(UID,
                          iso2,
                          iso3,
                          code3,
                          FIPS,
                          Admin2,
                          Province_State,
                          Country_Region,
                          Lat,
                          Long_,
                          Combined_Key , 
                          Population ) , 
                names_to = "date" , 
                values_to = "deaths" ) %>%
  mutate( date = lubridate::mdy(date)) %>%
  clean_names()

updated_covid_data_df <- 
  left_join(new_covid_case_numbers , new_covid_death_numbers) %>% 
  filter(iso3 == "USA" & iso2 == "US")

saveRDS( updated_covid_data_df , "data/raw/covid_county_update_6-26.rds")
# here i just wanted to get this higher-quality data into my update
#saveRDS( updated_covid_data_df , "data/export/us-county-data-updated-2020-06-25.rds")
