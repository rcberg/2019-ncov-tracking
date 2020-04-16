if(!require(pacman)) install.packages("pacman")
pacman::p_load( tidyverse ,
                lubridate , 
                jsonlite )

### get data


endpoint = "https://covidtracking.com/api/v1/states/current.json"

state_data = 
  fromJSON( endpoint ) %>%
  filter( is.na(score) == F )


### data 


state_test_data_reliable = 
  state_data %>%
  filter( score == 4 ) %>%
  select( state, 
          fips, 
          positive, 
          negative , 
          totalTestResults) %>%
  mutate( neg_rate = negative/totalTestResults ,
          rank = 1 + length(neg_rate) - rank(neg_rate) ,
          fips = as.numeric(fips) ) %>%
  rename(total_results = totalTestResults )

state_test_data = 
  state_data %>%
  select( state, 
          fips, 
          positive, 
          negative , 
          totalTestResults , 
          grade ) %>%
  mutate( neg_rate = negative/totalTestResults ,
          rank = 1 + length(neg_rate) - rank(neg_rate) ,
          GEOID = fips ) %>%
  rename(total_results = totalTestResults )

saveRDS(state_test_data , "data/export/states_testing_data.rds")

