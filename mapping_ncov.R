library(pacman)
p_load( tidyverse ,
        ggplot2  ,
        lubridate,
        sf ,
        maps ,
        gganimate)

setwd("D:/Economics/Projects/2019-ncov-tracking")


## 2019-nCov around the world

world_cases = readRDS("data/export/worldwide_data.rds") %>%
  filter( type == "Confirmed Cases" ) %>%
  select(-type )

world_cases_today = world_cases %>%
  filter( date == ymd(max(date)))


world  = st_as_sf(map("world", plot = FALSE, fill = TRUE)) %>% 
  st_transform( crs = 4326 )

proj_crs = st_crs(world)

world_cases_sf = world_cases_today %>% 
  st_as_sf( coords = c("long","lat") ,
            crs = 4326 )

gr = 
  st_graticule(lat = c(-89.9,seq(-80,80,20),89.9) , 
               crs = 4326)

world_plot = ggplot() + 
  geom_sf(data = world , fill = NA, col = "black", lwd = 0.3) +
  geom_sf(data = gr, color = "#cccccc", size = 0.15) + ## Manual graticule
  geom_point( data = world_cases_today %>% mutate( complete_cases = ifelse( cases > 0 , 
                                                                      cases  ,
                                                                      NA ) ), 
              aes( x = long , y = lat , size = complete_cases ) , 
              alpha = 0.4 , 
              color = 'red') + 
  coord_sf(datum = NA) +
  theme( panel.background = element_rect( fill = 'white')) + 
  labs(title = "2019 nCov Around the world")

world_dyn = world_plot + transition_states( date ) + ggtitle( '2019-nCov around the world' , 
                                                  subtitle = "{closest_state}")

animate( world_dyn , width = 900 , height = 900 , duration = 25 )

library(leaflet)
world_cases_sf %>% 
  filter( cases > 0 ) %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addCircleMarkers( radius = ~log(cases) , 
                    popup = ~paste0(paste(province_state, country_region , sep = ", ") , 
                                    "<br>", "Confirmed Cases: ", prettyNum(cases, big.mark=",")),
                    color = 'red' , 
                    stroke = F ) 
