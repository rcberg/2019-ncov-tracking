library(tidyverse)

recent_date <- Sys.Date() - lubridate::days()

state_ts_df <- 
  readRDS("data/export/state_covid_census_election.rds") %>%
  group_by( statefip, state_name ) %>%
  mutate( new_cases = confirmed - dplyr::lag(confirmed, order_by = date ) ,
          new_deaths = deaths - dplyr::lag(deaths, order_by = date ) ,
          virality = new_cases/confirmed ,
          incidence = confirmed/pop , 
          di_n_ratio = new_cases/pop ,
          dm_n_ratio = new_deaths/pop ,
          black_share = black/pop , 
          hispanic_share = hispanic/pop , 
          medicaid_share = insured_mcd/pop , 
          insured_share = insured/pop ,
          emp_pop_ratio = employed/pop , 
          avg_age = round(avg_age) ,
          southern = ifelse( region > 30 & region < 34 & !(state_name %in% c("Maryland","District of Columbia","Delaware")) ,
                          1 , 
                          0 ) 
          )

state_df <- 
  state_ts_df %>%
  filter( date %in% c(lubridate::ymd("2020-03-22") , 
                      lubridate::ymd("2020-03-22") + lubridate::days() , 
                      lubridate::ymd("2020-03-22") + lubridate::days(2) , 
                      lubridate::ymd("2020-03-22") + lubridate::days(3) , 
                      lubridate::ymd("2020-03-22") + lubridate::days(4) , 
                      lubridate::ymd("2020-03-22") + lubridate::days(5) , 
                      lubridate::ymd("2020-03-22") + lubridate::days(6) ,
                      recent_date, 
                      recent_date - lubridate::days() , 
                      recent_date - lubridate::days(2), 
                      recent_date - lubridate::days(3), 
                      recent_date - lubridate::days(4), 
                      recent_date - lubridate::days(5), 
                      recent_date - lubridate::days(6) )
  ) %>%
  mutate( recent = ifelse( date %in% c(recent_date, 
                                       recent_date - lubridate::days() , 
                                       recent_date - lubridate::days(2), 
                                       recent_date - lubridate::days(3), 
                                       recent_date - lubridate::days(4), 
                                       recent_date - lubridate::days(5), 
                                       recent_date - lubridate::days(6) ) ,
                          1 , 
                          0 ) 
  ) %>%
  group_by( statefip , 
            state_name , 
            state_abbr ,
            recent ) %>%
  summarise_all( mean, 
                 na.rm = T )

library(hrbrthemes)
early_late_cases <- 
  state_df %>% 
#  filter( recent == 1 ) %>%
  ggplot(aes(x = density , y = di_n_ratio ) ) + 
  geom_point() + 
  geom_smooth( method = 'lm' , se = F) + 
  scale_y_continuous( labels = scales::label_percent() ) +
  scale_x_continuous( labels = scales::comma_format()) +
  labs( y = "New cases as % of population" , 
        x = "Avg. population density*" , 
        caption = "*(Avg. pop. density calculated as population per sq. mi. around the average state resident)"  
        ) +
  facet_wrap(~recent , nrow = 1 , labeller = labeller( recent = c("0" = "Early" , "1" = "Recent"))) +
  theme_ipsum_rc( axis_title_size = 12 )

ggsave( early_late_cases , filename = "early_late_case_comparison_reg.png" , path = "reports/figures" )

early_late_deaths <- 
  state_df %>% 
  #  filter( recent == 1 ) %>%
  ggplot(aes(x = density , y = dm_n_ratio ) ) + 
  geom_point() + 
  geom_smooth( method = 'lm' , se = F) + 
  scale_y_continuous( labels = scales::label_percent() ) +
  scale_x_continuous( labels = scales::comma_format()) +
  labs( y = "New deaths as % of population" , 
        x = "Avg. population density*" , 
        caption = "*(Avg. pop. density calculated as population per sq. mi. around the average state resident)"  
  ) +
  facet_wrap(~recent , nrow = 1 , labeller = labeller( recent = c("0" = "Early" , "1" = "Recent"))) +
  theme_ipsum_rc( axis_title_size = 12 )

ggsave( early_late_deaths , filename = "early_late_death_comparison_reg.png" , path = "reports/figures" )

library(estimatr)
infect_fit <- 
  lm_robust( data = state_df , 
           formula = 
             di_n_ratio ~ gop_win*recent + density*recent + southern*recent ) 


deaths_fit <- 
  lm_robust( data = state_df , 
           formula = 
             dm_n_ratio ~ gop_win*recent + density*recent + avg_age*recent + southern*recent )

infect_fit %>%
  tidy() %>% 
  arrange(p.value)

deaths_fit %>%
  tidy() %>% 
  arrange(p.value)
