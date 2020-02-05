library(dplyr)
library(shiny)
library(igraph)
library(GGally)
library(stringr)
#library(plotly)
library(network)
library(tidyr)

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
    
    plot_gg <- eventReactive(input$bees,{
        
        selected_bee <- unlist(input$bees)
        
        nice_loc <- input$region
        
        fil_db <- db[db$locs == nice_loc,]
        
        fil2_db <- fil_db[fil_db$bee_sp %in% selected_bee,]
            
        if(length(selected_bee) == 1){
            
            bip_table <- data.frame(table(fil2_db[,c("bee_sp", "plant_sp")])) %>% 
                mutate(Freq = Freq*0.1) %>% 
                spread(key = bee_sp, value = Freq) %>% 
                mutate(` ` = 0) %>% 
                tibble::column_to_rownames("plant_sp") 
        }else{
            bip_table <- data.frame(table(fil2_db[,c("bee_sp", "plant_sp")])) %>% 
                mutate(Freq = Freq*0.1) %>% 
                spread(key = bee_sp, value = Freq) %>% 
                tibble::column_to_rownames("plant_sp")  
        }
            bip <- network(bip_table,
                           matrix.type = "bipartite",
                           ignore.eval = FALSE,
                           names.eval = "weights")
        
            col = c("actor" = "#50C878", "event" = "#FFAA1D")
            
            gg <- ggnet2(bip, label = TRUE, color = "mode", palette = col, edge.size = 'weights') +
                theme(legend.position = 'none')
        
        gg
    })

    # Make the plot
    output$plot1 <- renderPlot({
        plot_gg()
        })
   
    output$info <- renderText({
        ggp <- plot_gg()
        p_x <- input$plot_click$x
        p_y <- input$plot_click$y
        
        if (is.null(input$plot_click$x)){
            "Click on any species to see information about it."
        }else{
            min_x <- min(all_dist <- Rfast::dista(matrix(c(x = p_x, y = p_y), nrow =1), as.matrix(ggp$data[,c("x", "y")])))
            sp_name <- ggp$data[which(all_dist == min(all_dist)),"label"]
            sp_name2 <- str_replace(sp_name, "\n", " ")
            paste("selected species=",sp_name2, "\n")
        }
    })
    output$mySite <- renderUI({
        
        ggp <- plot_gg()
        p_x <- input$plot_click$x
        p_y <- input$plot_click$y
        
        if (is.null(input$plot_click$x)){
            " "
        }else{
            min_x <- min(all_dist <- Rfast::dista(matrix(c(x = input$plot_click$x, y = input$plot_click$y), nrow =1), as.matrix(ggp$data[,c("x", "y")])))
            sp_name <- ggp$data[which(all_dist == min(all_dist)),"label"]
            sp_name3 <- str_replace(sp_name, "\n", "_")
            sp_name2 <- str_replace(sp_name, "\n", " ")
            website <- paste0("https://en.wikipedia.org/wiki/", sp_name3)
            url <- a(sp_name2, href=website)
            tagList("Wikipedia link:", url)
        }
    })
    
})
