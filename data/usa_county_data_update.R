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

### plug in today's or yesterday's date, whichever has the data (if we update late)


recent_date = Sys.Date()

### gather past data to make the panel (not used in the map but great to have)

past_data = 
  readRDS( paste0("data/export/us-county-data-updated-" , recent_date-days(1) , ".rds" ,sep = "")
         ) 


### the JHU API endpoint attaches dates to the files very specifically so need to format that


todays_date_chr = 
  format(recent_date , "%m-%d-%Y")

todays_data_filename = 
  paste0(todays_date_chr, ".csv" , sep="")

### accessing JHU endpoint

todays_data_file = 
  paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
         todays_data_filename, sep="")

### formatting present dat data

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

### adding today's data to past data and saving locally

usa_counties = 
  bind_rows( todays_data , past_data )

saveRDS( usa_counties , paste0("data/export/us-county-data-updated-" , recent_date , ".rds" ,sep = "")
         )
