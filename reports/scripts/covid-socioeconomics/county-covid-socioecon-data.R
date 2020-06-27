library(tidyverse)


# covid data

covid_data <- 
  readRDS( "data/raw/covid_county_update_6-26.rds") %>% 
  rename( "county_name" = "combined_key" ) 

# election data

election_data <- 
  read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv") %>%
  rename( "fips" = "combined_fips" ) %>%
  select(votes_dem, votes_gop, total_votes, fips, state_abbr) %>%
  mutate( diff = votes_gop-votes_dem ,
          gop_win = ifelse(diff>0,1,0) ,
          size_win = abs(diff)/total_votes ) 

# master county dataset

county_covid_elections <- 
  covid_data %>%
  select(fips, county_name, date, lat, long, confirmed, deaths) %>%
  left_join( election_data %>% select(-state_abbr) )


partisan_covid_df <- 
  county_covid_elections %>%
  group_by( gop_win , date ) %>%
  summarise( confirmed = sum(confirmed) , 
             deaths = sum(deaths)  
  ) %>%
  mutate( 
    new_cases = confirmed - dplyr::lag(confirmed, order_by = date) , 
    new_deaths = deaths - dplyr::lag( deaths , order_by = date ) 
  )

saveRDS( partisan_covid_df , 
         "data/export/covid_by_party_county.rds")
