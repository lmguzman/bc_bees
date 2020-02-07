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
        
        type_net <- input$net_type
        
        if(type_net == "Pollinator"){
            
            t_bees <- table(fil_db$bee_sp)
            avail_bees <- sort(names(t_bees)[t_bees >1])
            
            if (is.null(avail_bees))
                avail_bees <- character(0)
            
            updateSelectInput(session, "bees",
                              label = "Pollinator species",
                              choices = avail_bees,
                              selected = NULL
            )
        }else{
            
            t_plants <- table(fil_db$plant_sp)
            
            avail_plants <- sort(names(t_plants)[t_plants >1])
            
            if (is.null(avail_plants))
                avail_plants <- character(0)
            
            updateSelectInput(session, "plants",
                              label = "Plant species",
                              choices = avail_plants,
                              selected = NULL)
            
        }
        
        
    })
    
    plot_gg <- eventReactive(c(
        input$net_type,
        input$bees,
        input$plants
    ),{
        
        type_net <- input$net_type
        
        if(type_net == "Pollinator"){
            selected_sp <- unlist(input$bees)
        }else{
            selected_sp <- unlist(input$plants)
        }
        
        nice_loc <- input$region
        
        fil_db <- db[db$locs == nice_loc,]
        
        
        if(!is.null(selected_sp)){
            
            if(type_net == "Pollinator"){
                fil2_db <- fil_db[fil_db$bee_sp %in% selected_sp,]
            }else{
                fil2_db <- fil_db[fil_db$plant_sp %in% selected_sp,]
            }
            
            if(length(selected_sp) == 1){
                
                
                if(type_net == "Pollinator"){
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
                    bip_table[2,] <- 0
                    rownames(bip_table)[2] <- ""
                }
                
                
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
            
            
        }

    })

    # Make the plot
    output$plot1 <- renderPlot({
        
        plot_gg()
        })
   
    
    sp_name_plot <- eventReactive(input$plot_click$x,{
        
        ggp <- plot_gg()
        p_x <- input$plot_click$x
        p_y <- input$plot_click$y
        min_x <- min(all_dist <- Rfast::dista(matrix(c(x = p_x, y = p_y), nrow =1), as.matrix(ggp$data[,c("x", "y")])))
        sp_name <- ggp$data[which(all_dist == min(all_dist)),"label"]
        
        sp_name
    })
    
    output$info <- renderText({

        if (is.null(input$plot_click$x)){
            "Click on any species to see information about it."
        }else{
            sp_name <- sp_name_plot()
            sp_name2 <- str_replace(sp_name, "\n", " ")
            paste("selected species=",sp_name2, "\n")
        }
    })
    output$mySite <- renderUI({
        if (is.null(input$plot_click$x)){
            " "
        }else{
            sp_name <- sp_name_plot()
            sp_name3 <- str_replace(sp_name, "\n", "_")
            sp_name2 <- str_replace(sp_name, "\n", " ")
            website <- paste0("https://en.wikipedia.org/wiki/", sp_name3)
            url <- a(sp_name2, href=website)
            tagList("Wikipedia link:", url)
        }
    })
    
    output$spImage <- renderImage({
        if (is.null(input$plot_click$x)){
            " " 
        }else{
            sp_name <- sp_name_plot()
            sp_name3 <- str_replace(sp_name, "\n", "_")
            filename <- normalizePath(file.path('./Images',
                                                paste(sp_name3, '.jpg', sep='')))
            return(list(
                src = filename,
                filetype = "image/jpeg",
                alt = "We currently don't have an image for this species."
            ))
        }
        
    }, deleteFile = FALSE)
    
})
