## Common Variants in Dogs from Different Countries of Origin

library(tidyverse)
library(janitor)
library(naniar)
library(ggmap)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(RColorBrewer)
library(rworldmap)
library(mapview)
library(sf)

new_dogs_joined <- read_csv("../../data/new_dogs_joined.csv")

spectral <- colorRampPalette(brewer.pal(8, "Spectral"))(23)

ui <- dashboardPage(
        dashboardHeader(title = "Dogs and Markers"),
        dashboardSidebar(disable = T),
        dashboardBody(
                
                fluidRow(
                        box(title = "Plot Options", width = 3,
                            selectInput("x", "Select Chromosome Position", choices = unique(dog_gwas_pivoted$chr_location), hr())
                        ), #closes the first box
                        box(title = "Dog Size and Markers by Country of Origin", width = 8,
                            plotOutput("plot", width = "500px", height = "400px")
                        ) #closes the second box
                ) #closes the row
        ) #closes the dashboard body
)
server <- function(input, output, session) {
        
        session$onSessionEnded(stopApp)
        
        output$plot <- renderPlot ({
                
                new_dogs_joined %>%
                        filter(marker_alleles_data != "NA") %>% 
                        filter(country_of_origin != "NA") %>% 
                        filter(chr_location == input$x) %>% 
                        ggplot(aes(x = country_of_origin, fill = country_of_origin)) +
                        geom_bar() +
                        coord_flip() +
                        facet_grid(marker_alleles_data~.) +
                        scale_fill_manual(values = spectral) +
                        theme_minimal() +
                        labs(title = "Variant by Country of Origin", 
                             x = "Country of Origin",
                             fill = "Country of Origin") +
                        theme(plot.title = element_text(size = rel(1.3), hjust = 0.5), axis.title.x = element_text(size = 0.5))
                
        })
        
}

shinyApp(ui, server)