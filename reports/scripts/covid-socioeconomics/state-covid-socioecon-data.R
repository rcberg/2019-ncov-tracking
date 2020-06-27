library(tidyverse)

# state id crosswalk

recent_date <- 
  Sys.Date()-lubridate::days()

library(rvest)
library(janitor)
site = "https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696"
state_name_fips = 
  read_html(site) %>%
  html_nodes("#detail > table") %>%
  html_table( fill = F ) %>%
  bind_rows() %>%
  clean_names() %>%
  rename( "state_abbr" = "postal_code" ,
          "statefip" = "fips" , 
          "state_name" = "name" ) %>%
  bind_rows( data.frame( state_name = "District of Columbia" , 
                         state_abbr = "DC" , 
                         statefip = 11))
# covid data

recent_data <- 
  readRDS( "data/raw/covid_county_update_6-26.rds") %>% 
  rename( "county_name" = "combined_key" ,
          "state_name" = "province_state" ) %>% 
  left_join( state_name_fips )

# election data

election_data <- 
  read_csv("https://raw.githubusercontent.com/tonmcg/US_County_Level_Election_Results_08-16/master/2016_US_County_Level_Presidential_Results.csv") %>%
  rename( "fips" = "combined_fips" ) %>%
  select(votes_dem, votes_gop, total_votes, fips, state_abbr) %>%
  mutate( diff = votes_gop-votes_dem ,
          gop_win = ifelse(diff>0,1,0) ,
          size_win = abs(diff)/total_votes ) %>%
  left_join( state_name_fips )

# census data, `usa_00011` is '16-'18 single-year samples and `usa_00012` is the '18 rolling 5yr sample. comment-out one ddi

library(ipumsr)
library(janitor)
acs_ddi <- 
  read_ipums_ddi("data/raw/usa_00011.xml")
#acs_ddi <- 
#  read_ipums_ddi("data/raw/usa_00012.xml")

acs_micro_data <- 
  read_ipums_micro( acs_ddi ) %>%
  clean_names() %>%
  filter( year == 2018 ) %>% #comment this out if using the rolling 5-year sample
  select(-c(indnaics, year, labforce)) %>%
  mutate_all(  as.numeric ) 
  
acs_state_data <- 
  acs_micro_data %>%
  mutate( hispanic = ifelse( hispan %in% c(0,9) , 
                             0 , perwt ) ,
          black = ifelse( race == 2 , 
                          perwt , 0 ) ,
          insured = ifelse(hcovany == 2 , 
                           perwt , 
                           0 ) ,
          insured_mcd = ifelse( hinscaid == 2 , 
                                perwt , 
                                0 ) ,
          insured_mcr = ifelse( hinscare == 2 , 
                                perwt , 
                                0 ) , 
          insured_emp = ifelse( hinsemp == 2,
                                perwt , 
                                0 ) ,
          insured_prv = ifelse( hinspur == 2 ,
                                perwt , 
                                0 ) , 
          employed = ifelse( empstat == 1 , 
                             perwt , 
                             0 ) , 
          unemployed = ifelse( empstat == 2 , 
                               perwt , 
                               0 ) , 
          inctot = na_if( inctot , 9999999) , 
          incwage = na_if( incwage , 9999999) , 
          ftotinc = na_if( ftotinc , 9999999)
  ) %>%
  group_by( statefip ) %>%
  summarise( pop = sum(perwt) , 
             region = mean(region) , 
             avg_age = mean(age) , 
             black = sum(black) , 
             hispanic = sum(hispanic) , 
             insured = sum(insured) , 
             insured_mcd = sum(insured_mcd) , 
             insured_mcr = sum(insured_mcr) , 
             insured_emp = sum(insured_emp) , 
             insured_prv = sum(insured_prv) , 
             employed = sum(employed) , 
             unemployed = sum(unemployed) , 
             inctot = weighted.mean(inctot, w = perwt , na.rm = T) , 
             incwage = weighted.mean( incwage , w = perwt , na.rm = T) , 
             ftotinc = weighted.mean( ftotinc , w = perwt , na.rm = T ) ,
             density = weighted.mean( density  , w = perwt , na.rm = T )
  )   

saveRDS( acs_state_data , 
         "data/export/acs_state_variables.rds")

# collapsing election data

election_state_data <- 
  election_data %>%
  group_by( statefip , state_name ) %>%
  summarise( votes_dem = sum(votes_dem) , 
             votes_gop = sum(votes_gop) , 
             total_votes = sum(total_votes) ,
             diff = sum(diff) 
             ) %>%
  mutate(
    gop_win = ifelse(diff>0,1,0) ,
    margin = abs(diff)/total_votes
  )

# collapsing covid data 

covid_state_data <- 
  recent_data %>%
  group_by( state_abbr, state_name , statefip , date ) %>%
  summarise( confirmed = sum(confirmed) , 
             deaths = sum(deaths) )

# master state dataset

state_df <- 
  acs_state_data %>%
  left_join( covid_state_data ) %>%
  left_join( election_state_data ) 

saveRDS( state_df , 
         "data/export/state_covid_census_election.rds")

# totals by 2016 vote

partisan_covid_df <- 
  state_df %>%
  group_by( gop_win , date ) %>%
  summarise( inctot = mean(inctot) , 
             confirmed = sum(confirmed) , 
             deaths = sum(deaths)  
             ) %>%
  mutate( 
    new_cases = confirmed - dplyr::lag(confirmed, order_by = date) , 
    new_deaths = deaths - dplyr::lag( deaths , order_by = date ) 
    )
saveRDS( partisan_covid_df , 
         "data/export/covid_by_party_state.rds")
