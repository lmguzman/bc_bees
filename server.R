library(shiny)
library(igraph)
library(RColorBrewer)


db <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    observe({
        
        # Can use character(0) to remove all choices
        nice_loc <- input$region
        
        fil_db <- db[db$locs == nice_loc,]
        
        avail_bees <- sort(unique(fil_db$bee_sp))
        
        if (is.null(avail_bees))
            avail_bees <- character(0)
        
        updateSelectInput(session, "bees",
                          label = "Bee species",
                          choices = avail_bees,
                          selected = NULL
        )
    })
    
    
    
    # Make the plot
    output$plot1 <- renderPlot({
        selected_bee <- input$bees
        
        nice_loc <- input$region
        
        fil_db <- db[db$locs == nice_loc,]
        
        fil2_db <- fil_db[fil_db$bee_sp == selected_bee,]
        
        plant_ints <- table(fil2_db$plant_sp)
        
        links <- data.frame(
            source=selected_bee,
            target=names(plant_ints),
            importance= plant_ints)
        
        nodes <- data.frame(
            name= c(selected_bee, names(plant_ints)),
            carac=c('bee', rep('plant', length(plant_ints))))
        
        network <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 
        
        # Make a palette of 3 colors
        coul  <- c("#FFAA1D", "#50C878")
        
        # Create a vector of color
        my_color <- coul[as.numeric(as.factor(V(network)$carac))]
        
        plot(network, vertex.color=my_color, edge.width=E(network)$importance.Freq, vertex.size = 50)    
        })
    
    output$info <- renderText({
        paste0("bee=", input$plot_click$x)
    })
    
})
