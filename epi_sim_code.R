if(!require(pacman)) install.packages("pacman")
p_load( 
  tidyverse ,
  hrbrthemes )


###############################################
## parameters
###############################################
##
## b = transmission rate in the population
## g = recovery rate in the population
## N = total population
##
###############################################

J = 100
n = 100000
b = 1.2
g = 0.55
s0 = 100000
i0 = 1

epi_plot = 
  function(s0, i0, J, n, b, g){
    
    sim_df = 
      tibble(
        s = rep(0, times = J) ,
        r = rep(0, times = J) , 
        i = rep(0, times = J) , 
        t = rep(0, times = J)
      )
    
    sim_df$s[1] = s0
    sim_df$r[1] = 0
    sim_df$i[1] = i0
    sim_df$t[1] = 1
    
    
    for( j in 2:J ){
      
      
      sim_df$s[j] = ( 1 - b*sim_df$i[(j-1)]/N )*sim_df$s[(j-1)]
      
      sim_df$r[j] = sim_df$r[(j-1)] + g*sim_df$i[(j-1)] 
      
      sim_df$i[j] = (1 + b*sim_df$s[(j-1)]/N - g)*sim_df$i[(j-1)]
      
      sim_df$t[j] = j
      
    }
    
    sim_df
   

ggplot( data = sim_data ) + 
  geom_line( aes( x = t , y = (i+r) ) ,
             size = 1 
             ) +
  geom_line( aes( x = t , y = r ) ,
             color = 'blue' , 
             size = 1 
             ) +
  geom_line( aes( x = t , y = i ) ,
             color = 'red' , 
             size = 1
             ) +
  geom_line( aes( x = t , y = s ) , 
             linetype = 2 
             ) +
  labs( title = "Disease cases" , 
        x = "Time since first case" , 
        y = "Number of individuals" 
        ) +
  theme_ipsum_rc( axis_title_size = 15 )

  }



epi_plot(s0 = 100000 , 
         i0 = 1 , 
         J = 100 , 
         n = 100000 , 
         b = 1.7 , 
         g = 0.55 )
