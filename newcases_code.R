library(tidyverse)

world_new_cases = 
  readRDS("data/export/worldwide_data.rds" ) %>%
  filter(type == "Confirmed Cases") %>%
  select(-type) %>%
  group_by( province_state , 
            country_region ) %>%
  arrange( date) %>%
  mutate( firstcase = 
            ifelse(cases > 99 , 1 , 0 ) , 
          time = cumsum(firstcase) , 
          new_cases = 
            cases - dplyr::lag(cases, order_by = date ) %>%
            replace_na( 1 ) 
  ) %>%
  filter( cases > 0 ) %>%
  select(-firstcase) 

world_new_deaths = 
  readRDS("data/export/worldwide_data.rds" ) %>%
  filter(type == "Death") %>% 
  select(-type) %>%
  group_by( province_state , 
            country_region ) %>%
  arrange( date) %>%
  mutate( firstcase = 
            ifelse(cases > 99 , 1 , 0 ) , 
          time = cumsum(firstcase) , 
          new_cases = 
            cases - dplyr::lag(cases, order_by = date ) %>%
            replace_na( 1 ) 
  ) %>%
  filter( cases > 0 ) %>%
  select(-firstcase) 


library(hrbrthemes)

country = "US"

world_new_cases %>% 
  filter( country_region == country ) %>%
  ggplot(aes(x = time , y = new_cases) ) + 
  geom_line( size = 1 ) + 
  geom_smooth( color = 'red') + 
  labs( title = paste("New COVID-19 cases: " , country , sep="") ,
        y = "Number of new cases" , 
        x = "Days since 100th case" ) +
  theme_ipsum_rc( axis_title_size = 12)

world_new_deaths %>% 
  filter( country_region == country ) %>%
  ggplot(aes(x = time , y = new_cases) ) + 
  geom_line( size = 1 ) + 
  geom_smooth( color = 'red') + 
  labs( title = paste("New COVID-19 deaths: " , country , sep="") ,
        y = "Number of new deaths" , 
        x = "Days since 100th case" ) +
  theme_ipsum_rc( axis_title_size = 12)
