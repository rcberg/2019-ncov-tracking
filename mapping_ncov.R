if(!require(pacman)) install.packages("pacman")
pacman::p_load( tidyverse ,
                ggplot2  ,
                lubridate,
                sf ,
                maps ,
                hrbrthemes , 
                plotly )


##########################
### 
###  here we take the
###  data that's pulled
###  and cleaned in 
###  the "data_update.R"
###  script and craft 
###  some informative
###  graphs and maps. 
###
##########################


# world data in general

world_data = readRDS("data/export/worldwide_data.rds")


## 2019-nCov around the world

world_cases = 
  world_data %>%
  filter( type == "Confirmed Cases" ) %>%
  select(-type )

world_deaths = 
  world_data %>%
  filter( type == "Death" ) %>%
  select(-type )

world_cases_today = 
  world_cases %>%
  filter( date == ymd(max(date)))

world_deaths_today = 
  world_deaths %>%
  filter( date == ymd(max(date)))

world  = 
  st_as_sf(map("world", plot = FALSE, fill = TRUE)) %>% 
  st_transform( crs = 4326 )

proj_crs = st_crs(world)

world_cases_sf = 
  world_cases_today %>% 
  st_as_sf( coords = c("long","lat") ,
            crs = 4326 )

world_deaths_sf = 
  world_deaths_today %>% 
  st_as_sf( coords = c("long","lat") ,
            crs = 4326 )

gr = 
  st_graticule(lat = c(-89.9,seq(-80,80,20),89.9) , 
               crs = 4326)

world_plot = ggplot() + 
  geom_sf(data = world , fill = NA, col = "black", lwd = 0.3) +
  geom_sf(data = gr, color = "#cccccc", size = 0.15) + ## Manual graticule
  geom_point( data = world_cases %>% mutate( complete_cases = ifelse( cases > 0 , 
                                                                      cases  ,
                                                                      NA ) ), 
              aes( x = long , y = lat , size = complete_cases ) , 
              alpha = 0.4 , 
              color = 'red') + 
  coord_sf(datum = NA) +
  theme( panel.background = element_rect( fill = 'white')) + 
  labs(title = "2019 nCov Around the world")


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

world_deaths_sf %>% 
  filter( cases > 0 ) %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addCircleMarkers( radius = ~log(cases) , 
                    popup = ~paste0(paste(province_state, country_region , sep = ", ") , 
                                    "<br>", "Deaths: ", prettyNum(cases, big.mark=",")),
                    color = 'red' , 
                    stroke = F ) 

## usa national data

usa_data_df = readRDS("data/export/usa_national_data.rds")

usa_cases = 
  usa_data_df %>% 
  filter( type == "Confirmed Cases") %>% 
  ggplot() + 
  geom_line( aes( x = date , y = cases ) ,
             color = 'red') + 
  labs( title = "2019 Novel Coronavirus in the United States" , 
        y = "Number of Confirmed Cases" , 
        x = "Date" ) + 
  theme_ipsum_rc( axis_title_size = 15 )

ggsave( "data/plots/usa_mar_16.png" , plot = usa_cases  )

usa_deaths = 
  usa_data_df %>% 
  filter( type == "Death") %>% 
  ggplot() + 
  geom_line( aes( x = date , y = cases ) ,
             size = 1 , 
             color = 'red') + 
  labs( title = "2019 Novel Coronavirus in the United States" , 
        y = "Number of Deaths" , 
        x = "Date" ) + 
  theme_ipsum_rc( axis_title_size = 15 )

ggsave( "data/plots/usa_mar_16_deaths.png" , usa_deaths )



## italy cases and deaths



italy_data =  
  world_data %>% 
  filter( country_region == "Italy")


### cases

          
italy_cases =  
  italy_data %>% 
  filter( type == "Confirmed Cases" )

italy_cases_plot = 
  italy_cases %>%
  ggplot() + 
  geom_line( aes( x = date , y = cases ) ,
             size = 1 , 
             color = 'red') + 
  labs( title = "2019 Novel Coronavirus in Italy" , 
        y = "Number of Confirmed Cases" , 
        x = "Date" ) + 
  theme_ipsum_rc( axis_title_size = 15 )

ggsave("data/plots/italy_mar18_cases.png", italy_cases_plot )


### deaths 


italy_deaths = 
  italy_data %>% 
  filter( type == "Death" )

italy_deaths_plot =  
  italy_deaths %>%
  ggplot() + 
  geom_line( aes( x = date , y = cases ) ,
             size = 1 , 
             color = 'red') + 
  labs( title = "2019 Novel Coronavirus in Italy" , 
        y = "Number of Deaths" , 
        x = "Date" ) + 
  theme_ipsum_rc( axis_title_size = 15 )

ggsave("data/plots/italy_mar18_deaths.png", italy_deaths_plot )

italy_plot =
  italy_data %>%
  ggplot() + 
  geom_line( aes( x = date , y = cases , 
                  color = as.factor(type) ) ,
             size = 1 ) + 
  labs( title = "2019 Novel Coronavirus in Italy" , 
        y = "Number of Deaths" , 
        x = "Date" ,
        color = "Type") +
  theme_ipsum_rc( axis_title_size = 15 )


## worldwide cases

country_cases = 
  world_cases %>% 
  group_by( country_region , date) %>%
  summarise( cases = sum(cases) )

country_deaths = 
  world_deaths %>% 
  group_by( country_region , date) %>%
  summarise( cases = sum(cases) ) 

cases_plot = 
  country_cases %>%
  ggplot( ) + 
  geom_line( aes( x = date , y = cases , color = country_region ) ) + 
  labs( title = "COVID-19 cases worldwide" , 
        x = "Date" , 
        y = " Confirmed Cases" , 
        color = "Country" ) +
  theme_ipsum_rc( axis_title_size = 15 )

deaths_plot = 
  country_deaths %>%
  ggplot( ) + 
  geom_line( aes( x = date , y = cases , color = country_region ) ) + 
  labs( title = "COVID-19 deaths worldwide" , 
        x = "Date" , 
        y = " Deaths" , 
        color = "Country" ) +
  theme_ipsum_rc( axis_title_size = 15 )

plotly::ggplotly( cases_plot
                  )

plotly::ggplotly( deaths_plot
)


## chinese cases

china_cases = 
country_cases %>% 
  filter( country_region == "China" ) %>%
  mutate( cases_L = dplyr::lag(cases) ,
          del = cases - cases_L )

china_cases %>% 
  ggplot(aes(x = date , y = del)) +
  geom_line( ) + geom_smooth( se = F ) + 
  labs( title = "Daily new cases in China" , 
        x = "Date" , 
        y = "Number of new cases", 
        caption = "Blue line is local regression estimate") + 
  theme_ipsum_pub( axis_title_size =  15)

