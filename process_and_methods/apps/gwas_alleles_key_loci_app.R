## Comparing Alleles in Key Loci in Small vs Large Dogs for ONLY GWAS: A Shiny App 

library(tidyverse)
library(shiny)
library(shinydashboard)
library(RColorBrewer)

dog_gwas <- read_csv(file = "../../data/dog_gwas.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Dogs and Markers"),
  dashboardSidebar(disable=T),
  dashboardBody(
    
    fluidRow(
      box(title = "Plot Options", width = 3,
          selectInput("x", "Select Chromosome Position", choices = unique(dog_gwas$chr_location), hr()),
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
    
    dog_gwas %>%
      filter(chr_location == input$x) %>% 
      ggplot(aes(x = height_category, fill = marker_alleles_data)) +
      geom_bar() +
      scale_fill_brewer(palette = "Spectral") +
      theme_minimal() +
      labs(title = "Largest vs Smallest Dogs",
           x = "Height Category",
           y = "Count",
           fill = "Marker") +
      theme(plot.title = element_text(size = rel(1.3), hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
  
}

shinyApp(ui, server)
