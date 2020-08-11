## load packages 

library(shiny)
library(shinydashboard)
library(rgdal) 
library(leaflet)
library(sp)
library(ggplot2)
library(ggmap)
library(cowplot)
library(dplyr)
library(purrr)
library(ggiraph) 
library(gridExtra)
library(htmlwidgets)
library(shinyalert)
library(shinyforms)
source("utils.R")


#reading in data and map data

db <- read.csv("data/site_net_loc_fil_links.csv", stringsAsFactors = FALSE)

nice_locations <- unique(db$ecosection_nm)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    
    # Application title and logo
    dashboardHeader(title= "Pollinators of \nBritish Columbia", titleWidth = 300,
                    tags$li(a(href = 'http://www.sfu.ca',
                              img(src = 'sfu_logo.png',
                                  title = "Company Home", height = "40px"),
                              style = "padding-top:5px; padding-bottom:5px;"),
                            class = "dropdown")),

    # Sidebar with a input for region, name and main action
    dashboardSidebar(width = 450, 
            # Plot with regions
            leafletOutput("plot_region", height = 300),
            
            # Alert of where do I start?
            useShinyalert(),  # Set up shinyalert
            actionButton("help", "Where do I start?"),
            selectInput(inputId = 'region',
                        label = 'Which region(s) are you interested in?',
                        choices = c("All", nice_locations)),
            selectInput(inputId = "name_type",
                        label = "How do you want to see species names?",
                        choices = c("Scientific names", "Common names")),
            selectInput(inputId = 'action_type',
                        label = 'What do you want to do?',
                        choices = c("", "Build Network", "Get plants", "Support crop")),
            conditionalPanel('input.action_type == "Build Network"',
                             selectInput(inputId = 'net_type',
                                         label = 'What type of network do you want to build?',
                                         choices = c("Pollinator", "Plant")),
                             
                             # conditional pannel for type of network once build network is selected
                             
                             conditionalPanel('input.net_type == "Pollinator"', 
                                              selectInput(inputId = 'bees',
                                                          label = 'Which pollinators do you want to include?',
                                                          choices = c("bee1", 'bee2'), 
                                                          multiple = TRUE)
                             ),
                             conditionalPanel('input.net_type == "Plant"', 
                                              selectInput(inputId = 'plants',
                                                          label = 'Which plants do you want to include?s',
                                                          choices = c("plant1", 'plant2'), 
                                                          multiple = TRUE)
                             )),
            # conditional panel for selecting get plants
            
            conditionalPanel('input.action_type == "Get plants"',
                             selectInput(inputId = 'maximizer',
                                         label = 'What feature do you want to maximize?',
                                         choices = c("Pollinator abundance", 'Pollinator diversity', 'Phenological coverage')),
                             checkboxGroupInput(inputId = "native",
                                                label = 'Do you want to use native plants only?',
                                                choices = c("Native", "Non-Native")),
                             checkboxGroupInput(inputId = "shrub",
                                                label = 'Which types of plants do you want to use?',
                                                choices = c("Herb","Shrub","Vine","Tree","Various")),
                             numericInput("n_plants", "How many plants do you want to choose?", 10, min = 2, max = 100),
                             actionButton("go", "Go!")),
            
            # conditional panel for selecting suppoting crop 
            
            conditionalPanel('input.action_type == "Support crop"',
                             selectInput(inputId = 'crop',
                                         label = 'Which crop do you want to support?',
                                         choices = c("Blueberry", 'Cranberry', 'Apple')),
                             checkboxGroupInput(inputId = "native_2",
                                                label = 'Do you want to use native plants only?',
                                                choices = c("Native", "Non-Native")),
                             checkboxGroupInput(inputId = "shrub_2",
                                                label = 'Which types of plants do you want to use?',
                                                choices = c("Herb","Shrub","Vine","Tree","Various")),
                             selectInput(inputId = "overlap_2",
                                                label = 'Should plants overlap with crop?',
                                                choices = c("Yes", "No")),
                             numericInput("n_plants_2", "How many plants do you want to choose?", 10, min = 4, max = 100),
                             actionButton("go2", "Go!"))
        ),

        # Show a plot of the generated distribution
        dashboardBody(
            
            # have 4 tabs
            tabsetPanel(type = "tabs",
                        # Tab for the main plot
                        
                        # Render girafe plot
                        tabPanel("Plot", fluidRow(girafeOutput("plot1")),
                                 # button for generating report
                                 fluidRow(downloadButton("report", "Generate report"))),
                        
                        # Tab for help 
                        tabPanel("Help", 
                                 h3("Which region(s) are you interested in?"),
                                 p("The map shows the regions where we currently have data from, and the names of the regions. You can select the region on the dropdown menu called 'Region'"),
                                 h3("How do you want to see species names?"),
                                 p("You can choose whether you want to see the plots and data reported using common names or scientific names. Common names will often aggregate multiple species into the same genus"),
                                 h3("What do you want to do?"),
                                 p("We currently have three main functionalities to the app:"),
                                 h4("Build Network"),
                                 p("You can choose individual plants or individual pollinators and see which species interact with that pollinator or plant. This option is useful if you want to see which plants do bumble bees interact with? or how can I support Bombus occidentalis?"),
                                 h4("Get plants"),
                                 p("This feature allows you to choose any number of plants that can maximize pollinator diversity (number of pollinator species supported), pollinator abundance (total number of pollinators supported) and phenological coverage (the most number of weeks of the year where plants are flowring and support the highest diversity)"),
                                 h4("Support crop"), 
                                 p("You can also choose a crop you want to support with a flower strip or a hedgerow. In BC we currently have the options for blueberries, cranberries and apples. Based on the crop you select and the features of the plants you select, we find the plants that have known pollinator visitors to those crops and then maximize the phenological coverage.")),
                        
                        # tab for feedback 
                        
                        tabPanel("Feedback", 
                                 h4("This app is a work in progress"),
                                  p("Here are some of the features we want to implement in the future:"),
                                  p("* Add more images of both the plants and pollinators"),
                                  p("* Expand the geographic range to include Western North America"),
                                  p("* Include citizen science data and other sources of data"),
                                  p(""),
                                   h4("If you have any feedback for us or would like to contribute data, please fill the following form"),
                                 formUI(formInfo)),
                        
                        ## tab for contributors 
                        tabPanel("Contributors and contact", 
                                 h4("Contact:"),
                                 p("Laura Melissa Guzman"),
                                 p("E-mail: laura_melissa_guzman@sfu.ca"),
                                 h4("Developers:"),
                                 p("This app is developed by Laura Melissa Guzman, Tyler Kelly, Melissa Platsko, Leithen M'Gonigle, Lora Morandin and Elizabeth Elle in collaboration with Pollination Partnership and the Native Bee Society of British Columbia."),
                                 h4("Data contributors:"),
                                 p("The data for this app was collected by Dr. Elizabeth Elle."),
                                 h4("Photo contributors:"),
                                 p("The photos for this app were collected by Sarah Jonhnson."),
                                 h4("References:"),
                                 p("This app uses an algorithm to calculate the phenological coverage, this algorithm was originally presented in:"),
                                 p("M'Gonigle, Williams, Lonsdorf, Kremen. (2016) A Tool for Selecting Plants When Restoring Habitat for Pollinators. Conservation Letters. 10(1): 105-111"),
                                 h4("Acknowledgements:"),
                                 p("We want to thank Sarah Jonhnson, Elijah Rejes, Claire Kremen, Carly McGregor, Matthew Pennell and the Native Bee Society of BC for feedback."))
                       
            )
        
         )
    )
)
