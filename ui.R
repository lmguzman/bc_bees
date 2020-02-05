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
            selectInput(inputId = 'bees',
                        label = 'Bee species',
                        choices = c("bee1", 'bee2'), 
                        multiple = TRUE)
            
        ),

        # Show a plot of the generated distribution
        dashboardBody(
            fluidRow(
                plotOutput("plot1", click= "plot_click")),
            fluidRow(
                verbatimTextOutput("info"),
                htmlOutput("mySite"))
        )
    )
)
