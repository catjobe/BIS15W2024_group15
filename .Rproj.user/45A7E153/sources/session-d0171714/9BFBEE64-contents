## Genetics and Dog Size: A Shiny App

library(tidyverse)
library(janitor)
library(naniar)
library(ggmap)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(RColorBrewer)

dog <- read_csv(file = "../../data/clean_dog_data.csv")

dog_long <- read_csv(file = "../../data/dog_long.csv")

dog_gwas <- read_csv("../../data/dog_gwas.csv")

dogs_joined <- read_csv("../../data/dogs_joined.csv")

new_dogs_joined <- read_csv("../../data/new_dogs_joined.csv")

spectral <- colorRampPalette(brewer.pal(8, "Spectral"))(23)
pie(rep(1, length(spectral)), col = spectral , main="") 

dog_gwas_pivoted <- dog_gwas %>% 
        mutate(height_category = case_when(height_cm <= 20.0 ~ "small",
                                           height_cm > 20.0 & height_cm < 73.0 ~ "medium",
                                           height_cm >= 73.0 ~ "large")) %>% 
        filter(height_category == "small" | height_category == "large") %>% 
        pivot_longer(cols = starts_with("chr15"),
                     names_to = "chr_location",
                     values_to = "marker_alleles_data")

dog_gwas_mass_pivoted <- dog_gwas %>% 
        mutate(mass_category = case_when(body_mass_kg <= 10.00 ~ "small",
                                         body_mass_kg > 10.00 & body_mass_kg < 30.11 ~ "medium",
                                         body_mass_kg >= 30.11 ~ "large")) %>% 
        filter(mass_category == "small" | mass_category == "large") %>% 
        pivot_longer(cols = starts_with("chr15"),
                     names_to = "chr_location",
                     values_to = "marker_alleles_data")

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Dogs Genetics - Size"),
                    dashboardSidebar(disable = T),
                    dashboardBody(
                            fluidPage(
                                    theme = shinytheme("sandstone"),
                                    tabsetPanel(
                                            tabPanel("Markers",
                                                     fluidRow(
                                                             box(title = "Plot Options", width = 3,
                                                                 selectInput("x", "Select Chromosome Position", choices = unique(dog_gwas_pivoted$chr_location), hr()),
                                                                 selectInput("y", "Select Sex", choices = c("M", "F"), selected = "M")
                                                             ), #closes the first box
                                                             box(title = "Dog Height and Markers", width = 8,
                                                                 plotOutput("plot1")
                                                             ) #closes the second box
                                                     ), #closes the first fluid row
                                                     fluidRow(
                                                             box(title = "Plot Options", width = 3,
                                                                 selectInput("x", "Select Chromosome Position", choices = unique(dog_gwas_pivoted$chr_location), hr()),
                                                                 selectInput("y", "Select Sex", choices = c("M", "F"), selected = "M")
                                                             ), #closes the box
                                                             box(title = "Dog Weight and Markers", width = 8,
                                                                 plotOutput("plot2")
                                                             ) #closes the box
                                                     ) #closes the fluid row
                                            ),  #closes the tab panel item
                                            tabPanel("IGF1 Genotype",
                                                     fluidRow(
                                                             box(title = "Mean Mass by IGF1 Genotype", width = 10,
                                                                 plotOutput("plot3")
                                                             ), #closes the box
                                                             box(title = "Table of Mean Mass by IGF1 Genotype", width = 10,
                                                                 tableOutput("table1")
                                                             ), #closes the box
                                                     ), #closes the fluidrow
                                                     fluidRow(
                                                             box(title = "Mean Height by IGF1 Genotype", width = 10,
                                                                 plotOutput("plot4")
                                                             ), #closes the box
                                                             box(title = "Table of Mean Height by IGF1 Genotype", width = 10,
                                                                 tableOutput("table2")
                                                             ) #closes the box
                                                     ) #closes the fluid row
                                            ), #closes the tab panel item
                                            tabPanel("Mean Mass and Heights by Country of Origin",
                                                     fluidRow(
                                                             box(title = "Mean Weight by Country of Origin", width = 10,
                                                                 plotOutput("plot5")
                                                             ) #closes the box
                                                     ), #closes the fluid row
                                                     fluidRow(
                                                             box(title = "Mean Height by Country of Origin", width = 10,
                                                                 plotOutput("plot6")
                                                             ) #closes the box
                                                     ) #closes the fluid row
                                            ) #closes the tab panel item
                                    ) #closes the tabset panel
                            ) #closes fluid page
                    ) #closes the dashboard body
) #closes the ui
server <- function(input, output, session) {
        output$plot1 <- renderPlot({ 
                dog_gwas_pivoted %>%
                        filter(sex == input$y) %>%
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
        }) #closes the render plot
        output$plot2 <- renderPlot({ 
                dog_gwas_mass_pivoted %>%
                        filter(sex == input$y) %>%
                        filter(chr_location == input$x) %>% 
                        ggplot(aes(x = mass_category, fill = marker_alleles_data)) +
                        geom_bar() +
                        scale_fill_brewer(palette = "Spectral") +
                        theme_minimal() +
                        labs(title = "Largest vs Smallest Dogs",
                             x = "Mass Category",
                             y = "Count",
                             fill = "Marker") +
                        theme(plot.title = element_text(size = rel(1.3), hjust = 0.5), axis.text.x = element_text(angle = 90, hjust = 1))
        }) #closes the render plot
        output$table1 <- renderTable({
                dog_gwas %>%
                        group_by(igf1_as_genotype) %>%
                        summarise(mean_body_mass = mean(body_mass_kg, na.rm=T), mean_height = mean(height_cm, na.rm=T))
        }) #closes the render table
        output$table1 <- renderTable({
                dog_gwas %>%
                        group_by(igf1_as_genotype) %>%
                        summarise(mean_body_mass = mean(body_mass_kg, na.rm=T)) %>% 
                        rename("Mean Mass (kg)" = mean_body_mass, "IGF1 Genotype" = igf1_as_genotype)
        }) #closes the render table
        output$plot3 <- renderPlot({
                dog_gwas %>%
                        group_by(igf1_as_genotype) %>%
                        summarise(meanbodymass = mean(body_mass_kg, na.rm=T)) %>%
                        ggplot(aes(x = igf1_as_genotype, y = meanbodymass, fill = igf1_as_genotype)) +
                        geom_col() +
                        scale_fill_brewer(palette = "Spectral") +
                        theme_minimal() +
                        labs(title = "Mean Mass by IGF1 Genotype", 
                             x = "IGF1 Genotype", 
                             y= "Mean Body Mass (kg)",
                             fill = "IGF1 Genotype") +
                        theme(plot.title = element_text(size = rel(1.3), hjust = 0.5))
        }) #closes the render plot
        output$table2 <- renderTable({
                dog_gwas %>%
                        group_by(igf1_as_genotype) %>%
                        summarise(mean_height = mean(height_cm, na.rm=T)) %>% 
                        rename("Mean Height (cm)" = mean_height, "IGF1 Genotype" = igf1_as_genotype)
        }) #closes the render table
        output$plot4 <- renderPlot({
                dog_gwas %>%
                        group_by(igf1_as_genotype) %>%
                        summarise(meanheight = mean(height_cm, na.rm=T)) %>%
                        ggplot(aes(x = igf1_as_genotype, y = meanheight, fill = igf1_as_genotype)) +
                        geom_col() +
                        scale_fill_brewer(palette = "Spectral") +
                        theme_minimal() +
                        labs(title = "Mean Height by IGF1 Genotype", 
                             x = "IGF1 Genotype", 
                             y = "Mean Height(cm)",
                             fill = "IGF1 Genotype")+
                        theme(plot.title = element_text(size = rel(1.3), hjust = 0.5))
        }) #closes the render plot
        output$plot5 <- renderPlot({
                new_dogs_joined %>% 
                        group_by(country_of_origin) %>%
                        summarize(mean_mass = mean(body_mass_kg, na.rm = T)) %>%
                        filter(country_of_origin != "NA") %>%
                        ggplot(aes(x = country_of_origin, y = mean_mass, fill = country_of_origin)) +
                        geom_col() +
                        coord_flip() +
                        scale_fill_manual(values = spectral)  +
                        theme_minimal() +
                        labs(title = "Mean Weight by Country of Origin",
                             x = "Country of Origin",
                             y = "Mean Mass (kg)",
                             fill = "Country of Origin") +
                        theme(plot.title = element_text(size = rel(1.3), hjust = 0.5))
        }) #closes the render plot
        output$plot6 <- renderPlot({
                new_dogs_joined %>% 
                        group_by(country_of_origin) %>%
                        summarize(mean_height = mean(height_cm, na.rm = T)) %>%
                        filter(country_of_origin != "NA") %>%
                        ggplot(aes(x = country_of_origin, y = mean_height, fill = country_of_origin)) +
                        geom_col() +
                        coord_flip() +
                        scale_fill_manual(values = spectral)  +
                        theme_minimal() +
                        labs(title = "Mean Height by Country of Origin",
                             x = "Country of Origin",
                             y = "Mean Height (cm)",
                             fill = "Country of Origin") +
                        theme(plot.title = element_text(size = rel(1.3), hjust = 0.5))
        }) #closes the render plot
        session$onSessionEnded(stopApp)
}
shinyApp(ui, server)