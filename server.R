library(dplyr)
library(shiny)
library(igraph)
library(GGally)
library(stringr)
#library(plotly)
library(network)
library(tidyr)
library(readr)
library(readxl)
library(rgdal)
library(leaflet)
library(sp)
library(ggplot2)
library(ggmap)
library(cartography)

db <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)

#reading in map data
sections <- readOGR("data/ERC_ECOSECTIONS_SP/ERC_ECOSEC_polygon.shp", stringsAsFactors = F)

sections@data <- sections@data %>%
  dplyr::rename("ecosection_cd" = ECOSEC_CD,
                "ecosection_nm" = ECOSEC_NM)

target2 <- c("SGI", "NAL", "LIM", "FRL", "OKR", "SOB", "NOB")
ecosec <- subset(sections, ecosection_cd %in% target2)


ecosec_data <- ecosec@data

ecosec@data <- ecosec@data %>%
  select(ecosection_cd) %>%
  mutate(ecosection_cd = factor(ecosection_cd))

ecosec_map <- spTransform(ecosec, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

eco_map <- leaflet(data = ecosec_map)

pal <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  output$plot_region <- renderLeaflet({

    eco_map %>%
    setView(lng = -122.5, lat = 49.2, zoom = 6) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(color = pal,
                popup = paste0("<strong>Ecosection: </strong>", ecosec_data$ecosection_nm))

    })
    observe({
        
        # Can use character(0) to remove all choices
        nice_loc <- input$region
        
        if(nice_loc == "All"){
            fil_db <- db
        }else{
            fil_db <- db[db$locs == nice_loc,]
        }

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
    
    maxi_plants <- eventReactive(input$go,{
        
        nice_loc <- input$region
        
        n_plants <- input$n_plants
        
        if(nice_loc == "All"){
            fil_db <- db
        }else{
            fil_db <- db[db$locs == nice_loc,]
        }
        
        if(input$maximizer == "Pollinator abundance"){
            
            pl_sp <- names(sort(table(fil_db$plant_sp), decreasing = TRUE)[1:n_plants])
        }else{
            
            tb <- table(fil_db$plant_sp, fil_db$bee_sp)
            tb[tb > 0] <- 1
            
            pl_sp <- names(sort(rowSums(tb), decreasing = TRUE)[1:n_plants])
        }
        
        
        fil2_db <- fil_db[fil_db$plant_sp %in% pl_sp,]
        
        bip_table <- data.frame(table(fil2_db[,c("bee_sp", "plant_sp")])) %>% 
            mutate(Freq = Freq*0.1) %>% 
            spread(key = bee_sp, value = Freq) %>% 
            tibble::column_to_rownames("plant_sp")  
        
        bip <- network(bip_table,
                       matrix.type = "bipartite",
                       ignore.eval = FALSE,
                       names.eval = "weights")
        
        col = c("actor" = "#50C878", "event" = "#FFAA1D")
        
        gg <- ggnet2(bip, label = TRUE, color = "mode", palette = col, edge.size = 'weights') +
            theme(legend.position = 'none')
        
        gg
        
    })
    
    plot_gg <- eventReactive(c(
        input$action_type,
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
        
        if(nice_loc == "All"){
            fil_db <- db
        }else{
            fil_db <- db[db$locs == nice_loc,]
        }
        
        
        
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
        if(input$action_type == "Build Network"){
           plot_gg()
        }else{
            maxi_plants()
        }
        })
   
    
    sp_name_plot <- eventReactive(input$plot_click$x,{
        
        if(input$action_type == "Build Network"){
            ggp <-plot_gg()
        }else{
            ggp <-maxi_plants()
        }
    
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

    
})
