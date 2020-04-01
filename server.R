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
library(cowplot)
library(tibble)
library(purrr)
source("R/functions.R")

db <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)

all_flowering_times <- readRDS("data/all_flowering_times.rds")

all_flying_times <- readRDS("data/all_flying_times.rds")

#reading in map data
sections <- readOGR("data/ERC_ECOSECTIONS_SP/ERC_ECOSEC_polygon.shp", stringsAsFactors = F)

sections@data <- sections@data %>%
  dplyr::rename("ecosection_cd" = ECOSEC_CD,
                "ecosection_nm" = ECOSEC_NM)

target2 <- c("SGI", "NAL", "LIM", "FRL", "OKR", "SOB", "SPR", "LIM", "HEL")
ecosec <- subset(sections, ecosection_cd %in% target2)


ecosec_data <- ecosec@data

ecosec@data <- ecosec@data %>%
  dplyr::select(ecosection_cd) %>%
  dplyr::mutate(ecosection_cd = factor(ecosection_cd))

ecosec_map <- spTransform(ecosec, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

eco_map <- leaflet(data = ecosec_map)

pal <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  output$plot_region <- renderLeaflet({

    eco_map %>%
    setView(lng = -123.8, lat = 49.8, zoom = 5.5) %>%
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
            fil_db <- db[db$ecosection_nm == nice_loc,]
        }
        
        names_to_use <- input$name_type
        
        if(names_to_use == "Common names"){
          fil_db$bee_sp <- fil_db$bee_common
          fil_db$plant_sp <- fil_db$plant_common
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
        
        if(nice_loc == "All"){
            fil_db <- db
        }else{
            fil_db <- db[db$ecosection_nm == nice_loc,]
        }
        
        plant_native <- if(all(input$native == "Native")){
          c("native", "both")
        }else if(all(input$native == "Non-Native")){
          c("non-native", "both")
        }else{
          c("native", "non-native", "both")
        }
        
        if(!is.null(plant_native)){
          fil_db <- fil_db[fil_db$plant_native %in% plant_native,]
        }
        
        if(!is.null(input$shrub)){
          fil_db <- fil_db[fil_db$plant_life_form %in% input$shrub,]
        }
        
        n_plants <- input$n_plants
        
        if(length(unique(fil_db$plant_sp)) < n_plants){
          n_plants <- length(unique(fil_db$plant_sp))
        }
        
        if(input$maximizer == "Pollinator abundance"){
            
            pl_sp <- names(sort(table(fil_db$plant_sp), decreasing = TRUE)[1:n_plants])
        }else if(input$maximizer == "Pollinator diversity"){
            
            tb <- table(fil_db$plant_sp, fil_db$bee_sp)
            tb[tb > 0] <- 1
            
            pl_sp <- names(sort(rowSums(tb), decreasing = TRUE)[1:n_plants])
        }else if(input$maximizer == "Phenological coverage"){
          
          flight.times.act <- all_flying_times[unique(fil_db$bee_sp)]
          
          bloom.times.act <- all_flowering_times[unique(fil_db$plant_sp)] 
          
          v.mat.act <- dplyr::select(fil_db, plant_sp, bee_sp) %>% 
            unique() %>% dplyr::mutate(int = 1) %>% 
            spread(key = 'plant_sp', value = int, fill = 0) %>% 
            tibble::column_to_rownames('bee_sp') %>% 
            as.matrix()
          
          #pl_sp <- find.mix(f=abundance.phenology.richness, k=n_plants, v.mat = v.mat.act, bloom.times = bloom.times.act, N = 100)
          withProgress(message = 'Making plot', value = 0, {
          
          n.gens = 1000
          x.in <- initial.popn(N = 100, n.plants = n_plants, n.plants.tot = ncol(v.mat.act),
                            fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act)
          out <- vector("numeric", n.gens)
          for ( i in seq_len(n.gens) ){
            x <- ga.step(N = 100, state = x.in, s = 5, p.mutate = 0.01, p.sex = 0.5, p.rec =  0.25, fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act)
            out[i] <- x$best.w
            
            incProgress(1/n.gens, detail = paste("Doing part", i))
           
          }
          res <- list(best.w=x$best.w, best.model=x$best.model, best.w.t=out)
          
          pl_sp <-colnames(v.mat.act)[which(res$best.model)]
          
          
          fil2_db <- fil_db[fil_db$plant_sp %in% pl_sp,]
          
          fil_bloom_times <- bloom.times.act[pl_sp] %>% 
            map_df(~data.frame(month = .x), .id = 'plant_sp') %>% 
            left_join(fil2_db) %>% 
            dplyr::select(plant_sp, month, plant_common) %>% 
            unique()
          
          })
        }
      
        
        fil2_db <- fil_db[fil_db$plant_sp %in% pl_sp,]
        
        names_to_use <- input$name_type
        
        if(names_to_use == "Common names"){
          fil2_db$bee_sp <- fil2_db$bee_common
          fil2_db$plant_sp <- fil2_db$plant_common
        }
        
        plant_order <- fil2_db %>% 
          dplyr::count(plant_sp) %>%
          arrange(n)
        
        if(input$maximizer == "Phenological coverage"){
          if(names_to_use == "Common names"){
            max_plot <- ggplot(fil_bloom_times) + geom_point(aes(x = month, y = plant_common), shape = 15, size = 10, colour = "#FCBA04") +
              theme_cowplot() + scale_x_continuous(limits = c(1,12), breaks = c(1:12), labels = c("Jan", "Feb", "Mar",
                                                                                                  "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
              xlab("") + ylab("")
          }
          max_plot <- ggplot(fil_bloom_times) + geom_point(aes(x = month, y = plant_sp), shape = 15, size = 10, colour = "#FCBA04") +
            theme_cowplot() + scale_x_continuous(limits = c(1,12), breaks = c(1:12), labels = c("Jan", "Feb", "Mar",
                                                                                                "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
            xlab("") + ylab("")
        }else{
          max_plot <- ggplot(fil2_db) + geom_bar(aes(x = plant_sp, fill = bee_guild)) + coord_flip() +
            theme_cowplot() + scale_fill_viridis_d(name = "Type of \n pollinator") +
            xlab("") + ylab("Number of recorded observations") + scale_x_discrete(limits = plant_order$plant_sp) 
        }
         
        max_plot
    })
    
    plot_gg <- eventReactive(c(
        input$action_type,
        input$net_type,
        input$bees,
        input$plants,
        input$name_type
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
            fil_db <- db[db$ecosection_nm == nice_loc,]
        }
        names_to_use <- input$name_type
        
        if(names_to_use == "Common names"){
          fil_db$bee_sp <- fil_db$bee_common
          fil_db$plant_sp <- fil_db$plant_common
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
           
          
          #net %v% "phono" = ifelse(letters[1:10] %in% c("a", "e", "i"), "vowel", "consonant")
          
          #data.frame(table(fil2_db[,c("bee_sp", "plant_sp")])) %>% 
            #left_join(select(fil2_db, plant_sp, plant_native)) %>% 
            #unique()
          
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
            p_x <- input$plot_click$x
            p_y <- input$plot_click$y
            min_x <- min(all_dist <- Rfast::dista(matrix(c(x = p_x, y = p_y), nrow =1), as.matrix(ggp$data[,c("x", "y")])))
            sp_name <- ggp$data[which(all_dist == min(all_dist)),"label"]
            
            sp_name
        }else{
            NULL
        }

    })
    
    output$info <- renderText({
      
      if(input$action_type == "Build Network"){
        if (is.null(input$plot_click$x)){
          "Click on any species to see information about it."
        }else{
          sp_name <- sp_name_plot()
          sp_name2 <- str_replace(sp_name, "\n", " ")
          paste("selected species=",sp_name2, "\n")
        }
      }
        
    })
    output$mySite <- renderUI({
      
      if(input$action_type == "Build Network"){
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
      }
    })

    
})
