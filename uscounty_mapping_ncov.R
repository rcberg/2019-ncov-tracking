if(!require(pacman)) install.packages("pacman")
pacman::p_load( tidyverse ,  
                sf , 
                lubridate ,
                tigris ,
                leaflet ,
                RColorBrewer )

recent_date = format(Sys.Date()-days(1) , "%m-%d-%Y")

#past_data = 
#  readRDS( paste0("data/export/us-county-data-updated-" , Sys.Date()-days(1) , ".rds" ,sep = "")
#  ) 

most_recent_data = 
  readRDS( paste0("data/export/us-county-data-updated-" , Sys.Date()-days(1) , ".rds" ,sep = "")
  ) %>%
  filter( last_update == recent_date ) %>%
  mutate( GEOID = ifelse( fips < 10000 , 
                          paste(0,fips,sep="") , 
                          fips ) 
          ) %>%
  select(GEOID , confirmed, deaths )

options(tigris_class = "sf")
us_counties_map = 
  tigris::counties(cb = T ) 

us_counties = 
  left_join( most_recent_data , us_counties_map ) %>%
  st_as_sf()



#ggplot(data = us_counties , aes(geometry = geometry)) + 
#  geom_sf(aes(fill = confirmed , color = confirmed ) ) + 
#  coord_sf(crs = 26910 ) + 
#  scale_fill_viridis_c(name = "Confirmed Cases", labels = scales::comma) + 
#  scale_color_viridis_c(name = "Confirmed cases", labels = scales::comma) +
#  labs( title = "Confirmed COVID-19 cases in the US")
  
col_pal_cases <- colorNumeric(palette = "YlOrRd", domain = log(1+us_counties$confirmed), n = 10)

cases_plot = 
  us_counties %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(
    popup = ~paste0(NAME, "<br>", "Confirmed cases: ", prettyNum(confirmed, big.mark=",")),
    stroke = FALSE,
    smoothFactor = 0,
    fillOpacity = 0.7,
    color = ~col_pal_cases(log(1+confirmed))
  ) %>%
  addLegend(
    "bottomright", 
    pal = col_pal_cases, 
    values = ~log(1+confirmed),
    title = "Confirmed Cases (Logarithm)",
    opacity = 1
  ) %>% 
  setView(lat=40, lng=-95, zoom=3.5 )

col_pal_deaths <- colorNumeric(palette = "YlOrRd", domain = log(1+us_counties$deaths), n = 10)

deaths_plot = 
  us_counties %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(
    popup = ~paste0(NAME, "<br>", "Deaths: ", prettyNum(deaths, big.mark=",")),
    stroke = FALSE,
    smoothFactor = 0,
    fillOpacity = 0.7,
    color = ~col_pal_deaths(log(1+deaths))
  ) %>%
  addLegend(
    "bottomright", 
    pal = col_pal_deaths, 
    values = ~log(1+deaths),
    title = "Deaths (Logarithm)",
    opacity = 1
  ) %>% 
  setView(lat=40, lng=-95, zoom=3.5 )
