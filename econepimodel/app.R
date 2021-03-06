#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(hrbrthemes)

epi_data_function = 
    function(s0, i0, J, n, b, g){
        
        sim_df = 
            tibble(
                s = rep(0, times = J) ,
                r = rep(0, times = J) , 
                i = rep(0, times = J) , 
                t = c(1:J)
            )
        
        sim_df$s[1] = (n-i0)
        sim_df$r[1] = 0
        sim_df$i[1] = i0
        sim_df$t[1] = 1
        
        
        for( j in 2:J ){
            
            
            sim_df$s[j] = ( 1 - b*sim_df$i[(j-1)]/n )*sim_df$s[(j-1)]
            
            sim_df$r[j] = sim_df$r[(j-1)] + g*sim_df$i[(j-1)] 
            
            sim_df$i[j] = (1 + b*sim_df$s[(j-1)]/n - g)*sim_df$i[(j-1)]
            
        }
        
        sim_df
    }

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("A Basic Epidemiological Model"),
    h5("Author: ", a("Chris Berg", href="https://twitter.com/rcberg", target="_blank")),
    h5("Source code: ", a("https://github.com/rcberg/2019-ncov-tracking/", href="https://github.com/rcberg/2019-ncov-tracking/tree/master/econepimodel", target="_blank")),
    

    # Sidebar with a slider
    sidebarLayout(
        sidebarPanel(
            
            sliderInput("b",
                        "Population transmission rate:",
                        min = 0.7,
                        max = 2,
                        value = 1.5 , 
                        step = 0.1 ) ,
            sliderInput("i0" , 
                        "Number of initially infected:",
                        min = 1 , 
                        max = 1000 ,
                        value = 1 
                        )
            , 
            sliderInput("J" , 
                        "Timespan:",
                        min = 75 , 
                        max = 200 ,
                        value = 100
                        ) 
            
        ),
        

        # Show a plot
        mainPanel(
           plotOutput("epi.plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    epi_plot = reactive({
        epi_data_function( 
            n = 100000 , 
            i0 = input$i0 , 
            J = input$J ,
            b = input$b , 
            g = 0.55 )
        
    })
    output$epi.plot <- renderPlot({
        
        
        ggplot( data = epi_plot() ) + 
            geom_line( aes( x = t , y = (i+r) , color = "Ever infected") ,
                       size = 1 
            ) + 
            geom_line( aes( x = t , y = r , color = "Recovered" ) ,
                       size = 1 
            ) +
            geom_line( aes( x = t , y = i , color = "Infected" ) ,
                       size = 1
            ) +
            geom_line( aes( x = t , y = s , color = "Susceptible") , 
                       linetype = 2 
            ) +
            scale_color_manual(name = "Epi. Variable" , 
                               values = c("Ever infected" = 'black' , 
                                          "Recovered" = 'blue' , 
                                          "Infected" = 'red' ,
                                          "Susceptible" = 'darkgrey')) + 
            labs( title = "Disease cases" , 
                  x = "Time since first case" , 
                  y = "Number of individuals" 
            ) +
            theme_ipsum_rc( axis_title_size = 15 )
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
