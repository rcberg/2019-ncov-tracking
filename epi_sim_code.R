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


epi_plot = 
  function(i0, J, n, b, g){
    
    sim_df = 
      tibble(
        s = rep(0, times = J) ,
        r = rep(0, times = J) , 
        i = rep(0, times = J) , 
        t = c(1:J)
      )
    
    sim_df$r[1] = 0
    sim_df$i[1] = i0
    sim_df$s[1] = (n-i0)
    sim_df$t[1] = 1
    
    
    for( j in 2:J ){
      
      
      sim_df$s[j] = ( 1 - b*sim_df$i[(j-1)]/n )*sim_df$s[(j-1)]
      
      sim_df$r[j] = sim_df$r[(j-1)] + g*sim_df$i[(j-1)] 
      
      sim_df$i[j] = (1 + b*sim_df$s[(j-1)]/n - g)*sim_df$i[(j-1)]
      
    }
    
    sim_df
}


sim_data = 
  epi_plot(
    J = 75 , 
    n = 100000 , 
    b = 2 , 
    g = 0.55 ,
    i0 = 1 
  )


baseline_plot = 
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


epi_policy_function = 
  function(i0, J, n, T_st , T_end , b_norm, b_lo ,  g){
    
    sim_df = 
      tibble(
        s = rep(0, times = J) ,
        r = rep(0, times = J) , 
        i = rep(0, times = J) , 
        t = c(1:J) ,
        b = ifelse( t < T_st | t > T_end , 
                    b_norm , 
                    b_lo )
      )
    
    sim_df$r[1] = 0
    sim_df$i[1] = i0
    sim_df$s[1] = (n-i0)
    sim_df$t[1] = 1
    
    
    for( j in 2:J ){
      
      sim_df$s[j] = ( 1 - sim_df$b[j]*sim_df$i[(j-1)]/n )*sim_df$s[(j-1)]
      
      sim_df$r[j] = sim_df$r[(j-1)] + g*sim_df$i[(j-1)] 
      
      sim_df$i[j] = (1 + sim_df$b[j]*sim_df$s[(j-1)]/n - g)*sim_df$i[(j-1)]
      
      
    }
    
    sim_df
  }

policy_df = 
  epi_policy_function(i0 = 1, 
                      J = 100, 
                      n = 100000, 
                      T_st = 12 , 
                      T_end = 30 , 
                      b_norm = 2 , 
                      b_lo = 0.7,  
                      g = 0.55 )


policy_plot = 
  ggplot( data = policy_df ) + 
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
  theme_ipsum_rc( axis_title_size = 12 )

policy_plot
