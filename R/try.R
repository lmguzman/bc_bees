input <- NULL

input$region <- "All"
input$name_type <- "Scientific names"
input$native <- c("Native")
input$n_plants <- 10
input$maximizer <- "Pollinator abundance"

#testing
pl_sp <- colnames(v.mat.act)[c(1,10, 50, 20)]

input$net_type <- "Plant"
input$net_type <- "Pollinator"

input$plants <- c("Abelia")
input$plants <- c("Abelia", "Mahonia\naquifolium", "Collinsia\nparviflora")
 
input$maximizer <-  "Pollinator abundance"
input$maximizer <-  "Phenological Coverage"
 
input$name_type <- "Scientific names"
input$bees <- c("Adela\nseptentrionella")

input$name_type <- "Common names"
input$bees <- c("Digger bees", "Mining bee", "Mason bees")
input$bees <- c("Digger bees")
sort(unique(db$bee_sp))





input$action_type <- "Build Network"
db %>% 
  filter(locs == "Cowichan", bee_sp == "Andrena\nvicina")


p <- plot(network, vertex.color=my_color, edge.width=E(network)$importance.Freq, vertex.size = 50)  


str(p)
p2 <- plot(1:10)

p2$x


input$plot_click$x <- 0.18
input$plot_click$y <- 0.88

xnew <- as.matrix( iris[1:10, 1:4] )
x <- as.matrix( iris[-c(1:10), 1:4] )
a <- Rfast::dista(xnew, x)




#How the data comes into the network figure 
# bip_table <- data.frame(table(db[,c("bee_sp", "plant_sp")])) %>% 
#   spread(key = bee_sp, value = Freq) %>% 
#   tibble::column_to_rownames("plant_sp")

#################################################################################
#Tylers stuff
#apply filter to db
# get_mat <- function(df) {
#   contin_tab <- as.data.frame.matrix(xtabs(~ plant_sp + bee_sp, df))
#   return(contin_tab)
# }

#filter the right data and convert to matrix
# bip_table <- db %>%
#   filter(plant_order == "Liliales") %>%
#   get_mat() %>%
#   rownames_to_column("plant_sp") %>%
#   naniar::replace_with_na_all(condition = ~.x == 1) %>% 
#   replace(is.na(.), 0) %>% 
#   tibble::column_to_rownames("plant_sp") %>% View()

#################################################################################
# #drop the ones from the filter table
# bip_table <- bip_table %>% 
#   rownames_to_column("plant_sp") %>%
#   naniar::replace_with_na_all(condition = ~.x == 1) %>% 
#   replace(is.na(.), 0) %>% 
#   tibble::column_to_rownames("plant_sp") 
# 
# #remove cols with no values
# bip_table <- bip_table[, -(which(colSums(bip_table)==0))]

bip_table <- plant1
bip_table <- plant3
bip_table <- poll1 
bip_table <- poll3


#Failed attempts to remove the extra dot 

#create network object
if(ncol(bip_table) == 2){
  bip_table <- bip_table %>%
    rownames_to_column("plant_common") %>%
    pivot_longer(-plant_common, "bee_sp", "count") %>%
    pivot_wider(names_from = "plant_common", values_from = value)
}


# df <- data.frame(groups = c("Native", "Introduced", "Both", "Insect"),
#                  that = c(1,2,3,4),
#                  this= rep(1,4))
#                   
# legend <- df %>% 
#   ggplot(., aes(that, this, label = groups)) +
#   geom_point(size = 8, col = c("#18b583","#f4a582","#fa9fb5", "#1092de")) +
#   ylim(.25, 2) +
#   xlim(0, 5) +
#   geom_text(, size = 5, nudge_y = .6) +
#   theme(axis.line=element_blank(),axis.text.x=element_blank(),
#         axis.text.y=element_blank(),axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),legend.position="none",
#         panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
#         panel.grid.minor=element_blank(),plot.background=element_blank())
#  
#  gg <- grid.arrange(gg, legend, nrow = 2, heights = c(2,.5))

install.packages("taxize")
library(taxize)

sci_names <- unique(str_extract(db$bee_sp, '[A-Za-z]+'))

bee_family <- tax_name(query = sci_names, get = "family")

bee_family <- bee_family %>%
  dplyr::select(-db, "bee_family" = family, "bee_genus"= query)

db %>%
  add_column(bee_genus = str_extract(db$bee_sp, '[A-Za-z]+'), .after = "bee_order") %>% 
  left_join(bee_family) %>%
  dplyr::select(1:11, length(.), everything()) %>% 
  dplyr::mutate(bee_family = case_when(bee_genus == "Adela" ~ "Adelidae",
                                       TRUE ~ bee_family))

###############################################################################
input <- NULL
input$region <- "All"
input$crop <- "Cranberry"
input$n_plants_2 <- 4
input$native_2 <- c("Native")
input$name_type <- "Common names"

crop_type <- input$crop

if(crop_type == 'Cranberry'){crop_type <- "Blueberry"}
if(crop_type == 'Apple'){crop_type <- "Apples"}

bees_crop <- unique(db[db$plant_common == crop_type,'bee_sp'])

fil_db_1 <- db[db$bee_sp %in% bees_crop,]

fil_db <- fil_db_1[fil_db_1$plant_common != crop_type,]
fil_db_crop <-fil_db_1[fil_db_1$plant_common == crop_type,]

plant_native <- if(all(input$native_2 == "Native")){
  c("native", "both")
}else if(all(input$native_2 == "Non-Native")){
  c("non-native", "both")
}else{
  c("native", "non-native", "both")
}

if(!is.null(plant_native)){
  fil_db <- fil_db[fil_db$plant_native %in% plant_native,]
}

if(!is.null(input$shrub_2)){
  fil_db <- fil_db[fil_db$plant_life_form %in% input$shrub_2,]
}

n_plants_2 <- input$n_plants_2

if(length(unique(fil_db$plant_sp)) < n_plants_2){
  n_plants_2 <- length(unique(fil_db$plant_sp))
}

flight.times.act <- all_flying_times[unique(fil_db$bee_sp)]

bloom.times.act <- all_flowering_times[unique(fil_db$plant_sp)] 
bloom.times.crop <- all_flowering_times[unique(fil_db_crop$plant_sp)]

v.mat.act <- dplyr::select(fil_db, plant_sp, bee_sp) %>% 
  unique() %>% dplyr::mutate(int = 1) %>% 
  spread(key = 'plant_sp', value = int, fill = 0) %>% 
  tibble::column_to_rownames('bee_sp') %>% 
  as.matrix()

withProgress(message = 'Making plot', value = 0, {
  
  n.gens = 1000
  x.in <- initial.popn(N = 100, n.plants = n_plants, n.plants.tot = ncol(v.mat.act),
                       fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act)
  out <- vector("numeric", n.gens)
  for(i in seq_len(n.gens)){
    x <- ga.step(N = 100, state = x.in, s = 5, p.mutate = 0.01, p.sex = 0.5, p.rec =  0.25, fitness=abundance.phenology.richness, v.mat = v.mat.act, bloom.times = bloom.times.act)
    out[i] <- x$best.w
    
    incProgress(1/n.gens, detail = paste("Doing part", i))
    
  }
  
})

res <- list(best.w=x$best.w, best.model=x$best.model, best.w.t=out)

pl_sp <-colnames(v.mat.act)[which(res$best.model)] #get list of best plants
"REMOVE" pl_sp <- colnames(v.mat.act)[c(1,10, 50, 20)]

fil2_db <- fil_db[fil_db$plant_sp %in% pl_sp,]

fil2_db_wcrop <- rbind(fil2_db, fil_db_crop)

bloom.times.act <-  c(bloom.times.crop, bloom.times.act[pl_sp])

pl_sp <- append(pl_sp, "Vaccinium\ncorymbosum")

fil_bloom_times <- bloom.times.act %>%
  map_df(~data.frame(week = .x), .id = 'plant_sp') %>%
  left_join(fil2_db_wcrop) %>%
  dplyr::select(plant_sp, week, plant_common) %>%
  unique()

fil_bloom_times <- fil_bloom_times %>% 
  mutate(category = case_when(plant_common == "Blueberry" ~ "Crop",
                              plant_common == "Apple" ~ "Crop",
                              plant_common != "Blueberry" ~ "Other"))

names_to_use <- input$name_type

if(names_to_use == "Common names"){
    max_plot <- ggplot(fil_bloom_times) + geom_point(aes(x = week, y = plant_common, colour = category), shape = 15, size = 10) +
      theme_cowplot() + 
      scale_x_continuous(limits = c(1,52), breaks = seq(1,52,4.5), labels = c("Jan", "Feb", "Mar",
                                                                              "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                         sec.axis = sec_axis(trans = ~ .,name = 'Week', breaks = seq(1, 52, 3))) +
    xlab("") + ylab("")
    max_plot <- max_plot + theme(legend.position = "none")
}else{
  max_plot <- ggplot(fil_bloom_times) + geom_point(aes(x = week, y = plant_sp, colour = categorgy), shape = 15, size = 10) +
    theme_cowplot() + scale_x_continuous(limits = c(1,52), breaks = seq(1,52,4.5), labels = c("Jan", "Feb", "Mar",
                                                                                              "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                                         sec.axis = sec_axis(trans = ~ .,name = 'Week', breaks = seq(1, 52, 3))) +
    xlab("") + ylab("")
  max_plot <- max_plot + theme(legend.position = "none")
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


###############################################################################

unique(db$bee_guild)

db %>%
  mutate(bee_guild = case_when(bee_common == "Sand wasps" ~ "otherhym",
                               TRUE ~  as.character(bee_guild))) %>%
  mutate(bee_guild_otro = case_when(bee_guild == "bombyliidae" ~ "Flower flies", 
                               bee_guild == "andrenidae" ~ "Mining bees",
                               bee_guild == "syrphidae" ~ "Flower flies",
                               bee_guild == "otherfly" ~ "Flies",
                               bee_common == "Bumble bees" ~ "Bumble bees",
                               bee_guild == "apidae" & bee_common != "Bumble bees" & bee_common != "Honey bee"~ "Other bees",
                               bee_common == "Honey bee" ~ "Honey bees",
                               bee_guild == "megachilidae" ~ "Mason & leafcutter bees",
                               bee_guild == "halictidae" ~ "Sweat bees",
                               bee_guild == "colletidae" ~ "Other bees",
                               bee_guild == "lepidoptera" ~ "Moths & Butterflies",
                               bee_guild == "coleoptera" ~ "Beetles",
                               bee_guild == "otherhym" ~ "Wasps",
                               bee_guild == "aves" ~ "Birds",
                               TRUE ~ "Uncommon visitors")) %>% View()
  




##############################################################################