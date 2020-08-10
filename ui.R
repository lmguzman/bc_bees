library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)
library(sp)
library(ggplot2)
library(ggmap)
library(cartography)
library(cowplot)
library(dplyr)
library(purrr)
library(ggiraph)
library(gridExtra)
library(htmlwidgets)
library(shinyalert)
library(shinyforms)
source("utils.R")
#reading in map data

db <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)

nice_locations <- unique(db$ecosection_nm)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    
    # Application title
    dashboardHeader(title= "Pollinators of \nBritish Columbia", titleWidth = 300,
                    tags$li(a(href = 'http://www.sfu.ca',
                              img(src = 'sfu_logo.png',
                                  title = "Company Home", height = "40px"),
                              style = "padding-top:5px; padding-bottom:5px;"),
                            class = "dropdown")),

    # Sidebar with a slider input for number of bins
    dashboardSidebar(width = 450, 
            leafletOutput("plot_region", height = 300),
            useShinyalert(),  # Set up shinyalert
            actionButton("help", "Where do I start?"),
            selectInput(inputId = 'region',
                        label = 'Region',
                        choices = c("All", nice_locations)),
            selectInput(inputId = "name_type",
                        label = "Do you want to use:",
                        choices = c("Scientific names", "Common names")),
            selectInput(inputId = 'action_type',
                        label = 'What do you want to do?',
                        choices = c("", "Build Network", "Get plants", "Support crop")),
            conditionalPanel('input.action_type == "Build Network"',
                             selectInput(inputId = 'net_type',
                                         label = 'Type of Network',
                                         choices = c("Pollinator", "Plant")),
                             
                             
                             conditionalPanel('input.net_type == "Pollinator"', 
                                              selectInput(inputId = 'bees',
                                                          label = 'Pollinator species',
                                                          choices = c("bee1", 'bee2'), 
                                                          multiple = TRUE)
                             ),
                             conditionalPanel('input.net_type == "Plant"', 
                                              selectInput(inputId = 'plants',
                                                          label = 'Plant species',
                                                          choices = c("plant1", 'plant2'), 
                                                          multiple = TRUE)
                             )),
            conditionalPanel('input.action_type == "Get plants"',
                             selectInput(inputId = 'maximizer',
                                         label = 'Maximize:',
                                         choices = c("Pollinator abundance", 'Pollinator diversity', 'Phenological coverage')),
                             checkboxGroupInput(inputId = "native",
                                                label = 'Use native plants only?',
                                                choices = c("Native", "Non-Native")),
                             checkboxGroupInput(inputId = "shrub",
                                                label = 'Which types of plants do you want to use?',
                                                choices = c("Herb","Shrub","Vine","Tree","Various")),
                             numericInput("n_plants", "Number of plants:", 10, min = 2, max = 100),
                             actionButton("go", "Go")),
            conditionalPanel('input.action_type == "Support crop"',
                             selectInput(inputId = 'crop',
                                         label = 'Crop:',
                                         choices = c("Blueberry", 'Cranberry', 'Apple')),
                             checkboxGroupInput(inputId = "native_2",
                                                label = 'Use native plants only?',
                                                choices = c("Native", "Non-Native")),
                             checkboxGroupInput(inputId = "shrub_2",
                                                label = 'Which types of plants do you want to use?',
                                                choices = c("Herb","Shrub","Vine","Tree","Various")),
                             selectInput(inputId = "overlap_2",
                                                label = 'Should plants overlap with crop?',
                                                choices = c("Yes", "No")),
                             numericInput("n_plants_2", "Number of plants:", 10, min = 4, max = 100),
                             actionButton("go2", "Go"))
        ),

        # Show a plot of the generated distribution
        dashboardBody(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Plot", fluidRow(girafeOutput("plot1")),
                                 fluidRow(downloadButton("report", "Generate report"))),
                        tabPanel("Help", 
                                 h4("Region"),
                                 h4("Scientific name")),
                        tabPanel("Feedback", 
                                 fluidRow(h4("This app is a work in progress"),
                                          p("Here are some of the features we want to implement in the future:"),
                                          p("- Add more images of both the plants and pollinators"),
                                          p("- Expand the geographic range to include Western North America"),
                                          p("- Include citizen science data"),
                                          p(""),
                                          h4("If you have any feedback for us or would like to contribute data, please fill the following form")),
                                 fluidRow(formUI(formInfo))),
                        tabPanel("Contributors", 
                                 h4("Developers:"),
                                 p("This app is developed by Laura Melissa Guzman, Tyler Kelly, Melissa Platsko, Leithen M'Gonigle, Lora Morandin and Elizabeth Elle in collaboration with Pollination Partnership and the Native Bee Society of British Columbia"),
                                 h4("Data contributors:"),
                                 p("The data for this app was collected by Dr. Elizabeth Elle"),
                                 h4("Photo contributors:"),
                                 p("The photos for this app were collected by Sarah Jonhnson"),
                                 h4("References:"),
                                 p("Genetic algorithm for phenological coverage:"),
                                 p("M'Gonigle, Williams, Lonsdorf, Kremen. (2016) A Tool for Selecting Plants When Restoring Habitat for Pollinators. Conservation Letters. 10(1): 105-111"),
                                 h4("Correspondence:"),
                                 p("Laura Melissa Guzman"),
                                 p("E-mail: laura_melissa_guzman@sfu.ca"),
                                 h4("Acknowledgements:"),
                                 p("We want to thank Sarah Jonhnson, Elijah Rejes, Claire Kremen, Carly McGregor, Matthew Pennell and the Native Bee Society of BC for feedback."))
                       
            )
        
         )
    )
)
