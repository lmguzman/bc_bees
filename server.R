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
library(sna)
library(Hmisc)
library(ggiraph)
library(gridExtra)
library(htmlwidgets)
library(forcats)
source("R/functions.R")
library(shinyalert)

db <- read.csv("data/site_net_loc_fil.csv", stringsAsFactors = FALSE)

all_flowering_times <- readRDS("data/all_flowering_times.rds")

all_flying_times <- readRDS("data/all_flying_times.rds")

#reading in map data
sections <- readOGR("data/ERC_ECOSECTIONS_SP/ERC_ECOSEC_polygon.shp", stringsAsFactors = F)

sections@data <- sections@data %>%
  dplyr::rename("ecosection_cd" = ECOSEC_CD,
                "ecosection_nm" = ECOSEC_NM)

target2 <- c("SGI", "NAL", "LIM", "FRL", "OKR", "SOB", "SPR", "LIM")
ecosec <- subset(sections, ecosection_cd %in% target2)


ecosec_data <- ecosec@data

ecosec@data <- ecosec@data %>%
  dplyr::select(ecosection_cd) %>%
  dplyr::mutate(ecosection_cd = factor(ecosection_cd))

ecosec_map <- spTransform(ecosec, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

eco_map <- leaflet(data = ecosec_map)

pal <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#004F2D")

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
    observeEvent(input$help, {
      # Show a modal when the button is pressed
      shinyalert(closeOnEsc = TRUE, closeOnClickOutside = TRUE, "Welcome to the BC pollinator app!", "This app allows you to interact with our most up to-date data on plants and pollinators of British Columbia. You can use this app to see which pollinators visit your favourite plant, find what plants maximize your pollinator diversity or find a set of plants that support your crop. If you need more help head to the 'help' tab")
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
          withProgress(message = 'Running algorithm', value = 0, {
          
          n.gens = 300
          x <- initial.popn(N = 100, n.plants = n_plants, n.plants.tot = ncol(v.mat.act),
                            fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act)
          out <- vector("numeric", n.gens)
          for ( i in seq_len(n.gens) ){
            x <- ga.step(N = 100, state = x, s = 5, p.mutate = 0.01, p.sex = 0.5, p.rec =  0.25, fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act)
            out[i] <- x$best.w
            
            incProgress(1/n.gens, detail = paste("Doing part", i))
           
          }
          res <- list(best.w=x$best.w, best.model=x$best.model, best.w.t=out)
          
          pl_sp <-colnames(v.mat.act)[which(res$best.model)]
          
          
          fil2_db <- fil_db[fil_db$plant_sp %in% pl_sp,]
          
          fil_bloom_times <- bloom.times.act[pl_sp] %>% 
            map_df(~data.frame(week = .x), .id = 'plant_sp') %>% 
            left_join(fil2_db) %>% 
            dplyr::select(plant_sp, week, plant_common) %>% 
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
            max_plot <- ggplot(fil_bloom_times) + geom_point(aes(x = week, y = plant_common), shape = 15, size = 10, colour = "#FCBA04") +
              theme_cowplot() + scale_x_continuous(limits = c(1,52), breaks = seq(1,52,4.5), labels = c("Jan", "Feb", "Mar",
                                                                                                        "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                                   sec.axis = sec_axis(trans = ~ .,name = 'Week', breaks = seq(1, 52, 3))) +
              xlab("") + ylab("")
          }else{
            max_plot <- ggplot(fil_bloom_times) + geom_point(aes(x = week, y = plant_sp), shape = 15, size = 10, colour = "#FCBA04") +
              theme_cowplot() + scale_x_continuous(limits = c(1,52), breaks = seq(1,52,4.5), labels = c("Jan", "Feb", "Mar",
                                                                                                        "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                                   sec.axis = sec_axis(trans = ~ .,name = 'Week', breaks = seq(1, 52, 3))) +
              xlab("") + ylab("")
          }
        }else{
          #fil2_db
          
          max_plot <- ggplot(fil2_db) + geom_bar(aes(x = plant_sp, fill = bee_guild_otro)) + coord_flip() +
            theme_cowplot() + scale_fill_manual(name = "Type of \n pollinator", breaks = c("Honey bees", "Bumble bees", "Mason & Leafcutter bees", "Mining bees", "Sweat bees", "Other bees", 
                                                           "Flower flies", "Flies", "Wasps", "Beetles", "Moths & Butterflies", "Birds"), 
                                                values=c("#08306b", "#08519c", "#2171b5", "#6baed6", "#9ecae1", "#deebf7", 
                                                         "#c7e9c0", "#a1d99b", "#41ab5d", "#238b45", "#006d2c", "#00441b")) +
            #scale_fill_viridis_d(name = "Type of \n pollinator") +
            xlab("") + ylab("Number of recorded observations") + scale_x_discrete(limits = plant_order$plant_sp)
        }
         
        max_plot <- max_plot +
          theme(axis.text = element_text(size = 20),
                axis.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 20))
        
        max_plot <- girafe(ggobj = max_plot, width_svg = 20, height_svg = 13) %>%
          girafe_options(.,
                         opts_tooltip(opacity = .7),
                         opts_zoom(min = .5, max = 4),
                         sizingPolicy(defaultWidth = "100%", defaultHeight = "300px"))
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
                spread(key = bee_sp, value = Freq) %>% 
                dplyr::mutate(` ` = 0) %>% 
                tibble::column_to_rownames("plant_sp")
            }else{
              bip_table <- data.frame(table(fil2_db[,c("bee_sp", "plant_sp")])) %>% 
                spread(key = bee_sp, value = Freq) %>% 
                tibble::column_to_rownames("plant_sp")
              bip_table[2,] <- 0
              rownames(bip_table)[2] <- ""
            }
            
          }else{
            bip_table <- data.frame(table(fil2_db[,c("bee_sp", "plant_sp")])) %>% 
              spread(key = bee_sp, value = Freq) %>% 
              tibble::column_to_rownames("plant_sp")  
          }
          
          net <- bip_table %>% 
            network(matrix.type = "bipartite", 
                    ignore.eval = FALSE, 
                    names.eval = "weights")
          
          
          #Plant attributes
          if(names_to_use == "Common names"){
            plant_att <- bip_table %>%
              rownames_to_column("plant_common") %>%
              left_join(db[,c("plant_common", "plant_native", "plant_life_form")]) %>%
              distinct(plant_common, .keep_all = TRUE) %>%
              dplyr::mutate(plant_native = capitalize(plant_native))
          } else{
            plant_att <- bip_table %>%
              rownames_to_column("plant_sp") %>%
              left_join(db[,c("plant_sp", "plant_native","plant_life_form")]) %>%
              distinct(plant_sp, .keep_all = TRUE) %>%
              dplyr::mutate(plant_native = capitalize(plant_native))
          }
          
          #Pollinator attributes
          if(names_to_use == "Common names"){
            if(type_net == "Pollinator"){
              insect_att <- bip_table %>%
                rownames_to_column("plant_common") %>%
                pivot_longer(-plant_common, "bee_common", values_to = "count") %>%
                filter(bee_common != " ") %>%
                left_join(db[,c("bee_common", "bee_diet", "bee_nest_location", "bee_guild")]) %>%
                distinct(bee_common, .keep_all = TRUE) %>%
                dplyr::mutate(bee_diet = capitalize(bee_diet)) %>%
                dplyr::mutate(group = case_when(bee_common == "BLANK" ~ "BLANK",
                                                TRUE ~ "Insect"))
            }else{
              insect_att <- bip_table %>%
                rownames_to_column("plant_common") %>%
                pivot_longer(-plant_common, "bee_common", values_to = "count") %>%
                filter(plant_common != "") %>%
                left_join(db[,c("bee_common", "bee_diet", "bee_nest_location", "bee_guild")]) %>%
                distinct(bee_common, .keep_all = TRUE) %>%
                dplyr::mutate(bee_diet = capitalize(bee_diet)) %>%
                dplyr::mutate(group = case_when(bee_common == "BLANK" ~ "BLANK",
                                                TRUE ~ "Insect"))
            }
          } else{ 
            if(type_net == "Pollinator"){
              insect_att <- bip_table %>%
                rownames_to_column("plant_sp") %>%
                pivot_longer(-plant_sp, "bee_sp", values_to = "count") %>%
                filter(bee_sp != " ") %>%
                left_join(db[,c("bee_sp", "bee_diet", "bee_nest_location", "bee_guild")]) %>%
                distinct(bee_sp, .keep_all = TRUE) %>%
                dplyr::mutate(bee_diet = capitalize(bee_diet)) %>%
                dplyr::mutate(group = case_when(bee_sp == "BLANK" ~ "BLANK",
                                                TRUE ~ "Insect"))
            } else {
              insect_att <- bip_table %>%
                rownames_to_column("plant_sp") %>%
                pivot_longer(-plant_sp, "bee_sp", values_to = "count") %>%
                filter(plant_sp != "") %>%
                left_join(db[,c("bee_sp", "bee_diet", "bee_nest_location", "bee_guild")]) %>%
                distinct(bee_sp, .keep_all = TRUE) %>%
                dplyr::mutate(bee_diet = capitalize(bee_diet)) %>%
                dplyr::mutate(group = case_when(bee_sp == "BLANK" ~ "BLANK",
                                                TRUE ~ "Insect"))
            }
          }
          
          #Set colour of nodes based on Phono
          col <- c("Native"= "#18b583", "Non-native"="#f4a582", "Insect"="#1092de", "Both" = "#fa9fb5")
          net %v% "phono" = c(plant_att$plant_native, insect_att$group)
          
          alp <- c("Pollenivore"= 1, "Herbivore"=1, "Predator"=1, "Parasite"= 1, "Detritivore"=1, 
                   "Herb" = 1, "Shrub" = 1, "Tree" = 1, "Vine" = 1)
          net %v% "life" = c(plant_att$plant_life_form, insect_att$bee_diet)
          
          #set edge attributes size and colour
          set.edge.attribute(net, "eSize", sqrt(net %e% "weights"))
          set.edge.attribute(net, "eColor", ifelse(net %e% "weights" > 80, "#525252", 
                                                   ifelse(net %e% "weights" < 3, "#d9d9d9", 
                                                          ifelse(net %e% "weights" > 30, "#737373", "#969696"))))
          
          ggdata <-ggnet2(net,
                          label = FALSE,
                          mode = "kamadakawai",
                          color = "phono",
                          alpha = "life",
                          alpha.node = alp,
                          color.legend = "Group",
                          edge.size = "eSize",
                          edge.color = "eColor",
                          palette = col,
                          size = 10)$data
          
          # ggdata$color[ggdata$label == " "] <- NA
          # ggdata$alpha[ggdata$label == " "] <- NA
          # ggdata$label[is.na(ggdata$color)] <- NA
          
          ggdata$sci_names <- str_extract(ggdata$label, '[A-Za-z]+')
          
          ggdata$onclick <- sprintf("window.open(\"%s%s\")",
                                    "http://en.wikipedia.org/wiki/", 
                                    as.character(ggdata$sci_names))
          
          #make plot
          gg <- ggnet2(net,
                 label = FALSE,
                 mode = "kamadakawai",
                 color = "phono",
                 color.legend = element_blank(),
                 edge.size = "eSize",
                 edge.color = "eColor",
                 palette = col) +
            geom_point(aes(color = color), size = 12, color = "white") +
            geom_point(aes(color = color), size = 12, alpha = .75) +
            geom_point_interactive(aes(color = color, tooltip = ggdata$alpha, data_id = ggdata$alpha, onclick = ggdata$onclick), size = 30) +
            geom_text(aes(label = label), color = "black", size = 9) + 
            labs(caption = "Click on circle to go Wikipedia page!") +
            theme(legend.text = element_text(color = "red", size = 30),
                  legend.position = c(.8, 0.0),
                  legend.direction = "horizontal",
                  plot.caption = element_text(color = "black", size = 20, hjust = 0))
          
          gg <- girafe(ggobj = gg, width_svg = 20, height_svg = 17) %>% 
            girafe_options(.,
                         opts_tooltip(opacity = .7),
                         opts_zoom(min = .5, max = 4),
                         sizingPolicy(defaultWidth = "100%", defaultHeight = "300px"))
          
          gg
          #opts_hover(css = "fill:red;stroke:orange;r:5pt;"))
          
          }

    })

    plot_crop <- eventReactive(ignoreNULL = TRUE, 
      input$go2,{
        
      crop_type <- input$crop
      
      if(crop_type == 'Cranberry'){crop_type <- "Blueberry"}
      if(crop_type == 'Apple'){crop_type <- "Apples"}
      
      bees_crop <- unique(db[db$plant_common == crop_type,'bee_sp'])
      
      fil_db_1 <- db[db$bee_sp %in% bees_crop,]
      
      fil_db_crop <-fil_db_1[fil_db_1$plant_common == crop_type,]
      
      plant_native <- if(all(input$native_2 == "Native")){
        c("native", "both")
      }else if(all(input$native_2 == "Non-Native")){
        c("non-native", "both")
      }else{
        c("native", "non-native", "both")
      }
      
      if(!is.null(plant_native)){
        fil_db_1 <- fil_db_1[fil_db_1$plant_native %in% plant_native,]
      }
      
      if(!is.null(input$shrub_2)){
        fil_db_1 <- fil_db_1[fil_db_1$plant_life_form %in% input$shrub_2,]
      }
      
      nice_loc <- input$region
      
      if(nice_loc == "All"){
        fil_db_1 <- fil_db_1
      }else{
        fil_db_1 <- fil_db_1[fil_db_1$ecosection_nm == nice_loc,]
      }
      
      n_plants_2 <- input$n_plants_2
      
      if(length(unique(fil_db_1$plant_sp)) < n_plants_2){
        n_plants_2 <- length(unique(fil_db_1$plant_sp))
      }
      
      if(input$overlap_2 == 'Yes'){
        
        fil_db <- fil_db_1[fil_db_1$plant_common != crop_type,]
      
        flight.times.act <- all_flying_times[unique(fil_db$bee_sp)]
        
        bloom.times.act <- all_flowering_times[unique(fil_db$plant_sp)]
        
        v.mat.act <- dplyr::select(fil_db, plant_sp, bee_sp) %>% 
          unique() %>% dplyr::mutate(int = 1) %>% 
          spread(key = 'plant_sp', value = int, fill = 0) %>% 
          tibble::column_to_rownames('bee_sp') %>% 
          as.matrix()
        
        withProgress(message = 'Running algorithm', value = 0, {
          
          n.gens = 300
          x <- initial.popn(N = 100, n.plants = n_plants_2, n.plants.tot = ncol(v.mat.act),
                               fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act)
          out <- vector("numeric", n.gens)
          for(i in seq_len(n.gens)){
            x <- ga.step(N = 100, state = x, s = 5, p.mutate = 0.01, p.sex = 0.5, p.rec =  0.25, fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act)
            out[i] <- x$best.w
            
            incProgress(1/n.gens, detail = paste("Doing part", i))
            
          }
          
        })
        
        
      }else if(input$overlap_2 == 'No'){
        
        if(!(crop_type %in% unique(fil_db_1$plant_common))){
          fil_db <- rbind(fil_db_1, fil_db_crop)
        }else{
          fil_db <- fil_db_1
        }
        
        crop_name <- unique(fil_db[fil_db$plant_common == crop_type,"plant_sp"])
        
        flight.times.act <- all_flying_times[unique(fil_db$bee_sp)]
        
        bloom.times.act <- all_flowering_times[unique(fil_db$plant_sp)]
        
        v.mat.act <- dplyr::select(fil_db, plant_sp, bee_sp) %>% 
          unique() %>% dplyr::mutate(int = 1) %>% 
          spread(key = 'plant_sp', value = int, fill = 0) %>% 
          tibble::column_to_rownames('bee_sp') %>% 
          as.matrix()
        
        withProgress(message = 'Running algorithm', value = 0, {
          
          n.gens = 300
          x <- initial.popn.2(N = 100, n.plants = n_plants_2, n.plants.tot = ncol(v.mat.act),
                               fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act, crop = crop_name)
          out <- vector("numeric", n.gens)
          for(i in seq_len(n.gens)){
            x <- ga.step.2(N = 100, state = x, s = 5, p.mutate = 0.01, p.sex = 0.5, p.rec =  0.25, fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act, crop = crop_name)
            out[i] <- x$best.w
            
            incProgress(1/n.gens, detail = paste("Doing part", i))
            
          }
          
        })
        
      }
      
      bloom.times.crop <- all_flowering_times[unique(fil_db_crop$plant_sp)]
      
       res <- list(best.w=x$best.w, best.model=x$best.model, best.w.t=out)

        pl_sp <-colnames(v.mat.act)[which(res$best.model)]

        fil2_db <- fil_db[fil_db$plant_sp %in% pl_sp,]
        
        fil2_db_wcrop <- rbind(fil2_db, fil_db_crop)
        
        bloom.times.act <- c(bloom.times.crop, bloom.times.act[pl_sp])
        
        pl_sp <- append(pl_sp, names(bloom.times.crop)[1])

        fil_bloom_times <- bloom.times.act %>%
          map_df(~data.frame(week = .x), .id = 'plant_sp') %>%
          left_join(fil2_db_wcrop) %>%
          dplyr::select(plant_sp, week, plant_common) %>%
          unique()
        
        fil_bloom_times <- fil_bloom_times %>% 
          dplyr::mutate(category = case_when(plant_common == "Blueberry" ~ "Crop",
                                      plant_common == "Apples" ~ "Crop",
                                      plant_common != "Blueberry" ~ "Other"))
        
        names_to_use <- input$name_type
        
        
        if(names_to_use == "Common names"){
          
          max_plot <- ggplot(fil_bloom_times) + 
            geom_point(aes(x = week, y = plant_common, colour = category), shape = 15, size = 10) +
            theme_cowplot() + 
            scale_x_continuous(limits = c(1,52), breaks = seq(1,52,4.5), 
                               labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                               sec.axis = sec_axis(trans = ~ .,name = 'Week', breaks = seq(1, 52, 3))) +
            xlab("") + ylab("")
          
          max_plot <- max_plot + 
            scale_color_manual(values = c("Crop" = "#3182bd", "Other" = "#2ca25f")) + 
            theme(legend.position = "none")
        
          }else{
          max_plot <- ggplot(fil_bloom_times, palette = col) + 
            geom_point(aes(x = week, y = plant_sp, colour = category), shape = 15, size = 10) +
            theme_cowplot() + 
            scale_x_continuous(limits = c(1,52), breaks = seq(1,52,4.5), 
                               labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                sec.axis = sec_axis(trans = ~ .,name = 'Week', breaks = seq(1, 52, 3))) +
            xlab("") + ylab("")
          
          max_plot <- max_plot + 
            scale_color_manual(values = c("Crop" = "#3182bd", "Other" = "#2ca25f")) + 
            theme(legend.position = "none")
          
        }
        
        max_plot <- max_plot +
          theme(axis.text = element_text(size = 20),
                axis.title = element_text(size = 20),
                legend.text = element_text(size = 20),
                legend.title = element_text(size = 20))
        
        max_plot <- girafe(ggobj = max_plot, width_svg = 20, height_svg = 13) %>%
          girafe_options(.,
                         opts_tooltip(opacity = .7),
                         opts_zoom(min = .5, max = 4),
                         sizingPolicy(defaultWidth = "100%", defaultHeight = "300px"))
        max_plot
    })
    
    # Make the plot
    output$plot1 <- renderGirafe({
        if(input$action_type == "Build Network"){
          
          plot_gg()
          
        }else if(input$action_type == "Get plants"){
          maxi_plants()
        }else if(input$action_type == "Support crop"){
          plot_crop()
        }
        })
    

    
})
