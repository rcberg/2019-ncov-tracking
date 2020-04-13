if(!require(pacman)) install.packages("pacman")
pacman::p_load( tidyverse , 
                janitor ,
                lubridate )

# this little guy will come in handy later 
# (JHU records unassigned state cases to atlantic ocean; re-assigning to state centroid)

unassigned_states_coords = 
  data.frame( province_state = state.name , lat_correct = state.center$y , long_correct = state.center$x) %>%
  mutate( country_region = "US" ,
          admin2 = "Unassigned" ,
          province_state = as.character(province_state))

## constructing county data for the united states
## begins on 3-22-2020
#
#init_date_chr = "03-22-2020"
#
#init_data_filename = 
#  paste0(init_date_chr, ".csv" , sep="")
#
#init_data_file = 
#  paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
#         init_data_filename, sep="")
#
#init_data = 
#  read_csv(init_data_file) %>% 
#  clean_names() %>% 
#  filter( country_region == "US") %>%
#  mutate( fips = as.numeric(fips) )
#
#time_since_init = 
#  interval( mdy(init_date_chr) ,
#            Sys.Date() -1        
#  ) %>% 
#  time_length( unit = "day")
#
#past_data = 
#  init_data %>% 
#  mutate(last_update = init_date_chr)
#
#for(i in 1:time_since_init){
#  
#  date_i_chr = format( Sys.Date() - days(i) , "%m-%d-%Y")
#  
#  date_i_filename =  
#    paste0(date_i_chr, ".csv" , sep="")
#  
#  date_data_file = 
#    paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
#           date_i_filename, sep="")
#  
#  date_counties = read_csv(date_data_file) %>% 
#    clean_names() 
#  
#  date_counties = date_counties %>% 
#    filter( country_region == "US" ) %>%
#    mutate( fips = as.numeric(fips) , 
#            last_update = date_i_chr )
#  
#  updated_counties = 
#    bind_rows( date_counties , past_data )
#  
#  past_data = updated_counties
#  
#}
#
#saveRDS( past_data , paste0("data/export/us-county-data-updated-" , (Sys.Date()-days(1)) , ".rds" ,sep = "")
#)

past_data = 
  readRDS( paste0("data/export/us-county-data-updated-" , Sys.Date()-days(1) , ".rds" ,sep = "")
         ) 

todays_date_chr = 
  format(Sys.Date() , "%m-%d-%Y")

todays_data_filename = 
  paste0(todays_date_chr, ".csv" , sep="")

todays_data_file = 
  paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
         todays_data_filename, sep="")

todays_data = 
  read_csv(todays_data_file) %>%
  clean_names() %>% 
  filter( country_region == "US" ) %>%
  mutate( fips = as.numeric(fips) , 
          last_update = todays_date_chr ) %>%
  left_join( unassigned_states_coords) %>%
  mutate( lat = ifelse( is.na(fips) == T , 
                        lat_correct , 
                        lat ) ,
          long = ifelse( is.na(fips) == T , 
                         long_correct , 
                         long ) ,
          last_update = mdy(last_update)) %>%
  select( -c(lat_correct , long_correct) )

usa_counties = 
  bind_rows( todays_data , past_data )

saveRDS( usa_counties , paste0("data/export/us-county-data-updated-" , Sys.Date() , ".rds" ,sep = "")
         )
