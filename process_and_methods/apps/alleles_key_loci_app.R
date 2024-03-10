## Comparing Alleles in Key Loci in Small vs Large Dogs: A Shiny App 

library(tidyverse)
library(shiny)
library(shinydashboard)

dog <- read_csv(file = "../../dog_breeds_data/dog_long.csv")

ui <- dashboardPage(
        dashboardHeader(title = "Dogs and Markers"),
        dashboardSidebar(disable=T),
        dashboardBody(
                
                fluidRow(
                        box(title = "Plot Options", width = 3,
                            selectInput("x", "Select Chromosome Position", choices = unique(dog_long$chr_location), hr()),
                        ), #closes the first box
                        box(title = "Dog Size and Markers", width = 8,
                            plotOutput("plot", width = "500px", height = "400px")
                            
                        ) #closes the second box
                ) #closes the row
        ) #closes the dashboard body
) #closes the ui 

server <- function(input, output, session) {
        
        session$onSessionEnded(stopApp)
        
        output$plot <- renderPlot ({
                
                dog_long %>%
                        filter(chr_location == input$x) %>% 
                        ggplot(aes(x = height_category, fill = marker_alleles_data)) +
                        geom_bar() +
                        labs(title = "Largest vs Smallest Dogs",
                             x = "Height Category",
                             y = "Count",
                             fill = "Marker") +
                        theme(plot.title = element_text(size = rel(1.3), hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))
                
        })
        
}

shinyApp(ui, server)