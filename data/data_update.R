if(!require(pacman)) install.packages("pacman")
pacman::p_load( tidyverse , 
                janitor ,
                lubridate )


################################################
###
###  this code will pull data on cases of the
###  2019 novel coronavirus from the john's
###  hopkins university Github repo which is 
###  maintained/updated daily by ryan lau.
### 
###  code for graphs/maps using this
###  data is in the "mapping_ncov.R" script. 
###
################################################


## confirmed cases

raw_confirmed_data = 
  read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

confirmed_data_df = 
  raw_confirmed_data  %>%
  pivot_longer( -c("Province/State":"Long") , 
                names_to = "date" , 
                values_to = "cases" ) %>%
  mutate( type = "Confirmed Cases" , 
          date = mdy(date) )

## death cases

raw_deaths_data = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")


deaths_data_df = 
  raw_deaths_data  %>%
  pivot_longer( -c("Province/State":"Long") , 
                names_to = "date" , 
                values_to = "cases" ) %>%
  mutate( type = "Death" , 
          date = mdy(date) )

## grand dataset 

data_df = 
  confirmed_data_df %>%
  bind_rows( deaths_data_df
             ) %>% 
  clean_names() %>%
  mutate(province_state = replace_na(province_state , "Nationwide") ) %>%
  arrange( country_region ,
           desc(date) )

saveRDS( data_df , "data/export/worldwide_data.rds" )

## united states datasets

# us nationwide data

usa_data_df = 
  data_df %>% 
  filter( country_region == "US" )

saveRDS( usa_data_df , "data/export/usa_national_data.rds" )
