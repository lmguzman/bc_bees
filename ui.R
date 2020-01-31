library(shiny)

db <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)

nice_locations <- unique(db$locs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Pollinators of British Columbia"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'region',
                        label = 'Regions',
                        choices = nice_locations),
            selectInput(inputId = 'bees',
                        label = 'Bee species',
                        choices = c("bee1", 'bee2'))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("plot1", click= "plot_click"),
            verbatimTextOutput("info")
        )
    )
))
