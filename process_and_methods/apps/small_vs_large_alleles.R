## Top 5 Small vs Large Dogs Compared by Alleles in Key Loci for Determining Dog Size: A Shiny App 

library(tidyverse)
library(shiny)
library(shinydashboard)

dog <- read_csv(file = "../../data/clean_dog_data.csv")

ui <- dashboardPage(
        dashboardHeader(title = "Dogs and Markers"),
        dashboardSidebar(disable=T),
        dashboardBody(
                
                fluidRow(
                        box(title = "Plot Options", width = 3,
                            radioButtons("x", "Select Height Category", choices = c("small", "large"), selected = "large"),
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
                
                dog %>%
                        mutate(height_category = case_when(height_cm <= 20.0 ~ "small",
                                                           height_cm > 20.0 & height_cm < 73.0 ~ "medium",
                                                           height_cm >= 73.0 ~ "large")) %>% 
                        filter(height_category == "small" | height_category == "large") %>% 
                        pivot_longer(cols = starts_with("chr15"),
                                     names_to = "chr_location",
                                     values_to = "marker_alleles_data") %>% 
                        filter(height_category == input$x) %>% 
                        ggplot(aes(x = chr_location, fill = marker_alleles_data)) +
                        geom_bar() +
                        facet_wrap(breed~.) +
                        labs(title = "Dogs Compared by Alleles at 10 Positions",
                             x = "Position",
                             y = "Count",
                             fill = "Marker") +
                        theme(plot.title = element_text(size = rel(1.3), hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))
                
        })
        
}

shinyApp(ui, server)