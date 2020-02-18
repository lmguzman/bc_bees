library(shiny)
library(shinydashboard)

db <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)

nice_locations <- unique(db$locs)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(

    # Application title
    dashboardHeader(title= "Pollinators of \nBritish Columbia", titleWidth = 300),

    # Sidebar with a slider input for number of bins
    dashboardSidebar(
            selectInput(inputId = 'region',
                        label = 'Region',
                        choices = nice_locations),
            selectInput(inputId = 'action_type',
                        label = 'What do you want to do?',
                        choices = c("", "Build Network", "Get plants")),
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
                                         choices = c("Pollinator abundance", 'Pollinator diversity')),
                             numericInput("n_plants", "Number of plants:", 10, min = 1, max = 100),
                             actionButton("go", "Go"),)
        ),

        # Show a plot of the generated distribution
        dashboardBody(
            fluidRow(
                plotOutput("plot1", click= "plot_click")),
            fluidRow(
                verbatimTextOutput("info"),
                htmlOutput("mySite")),
        )
    )
)
