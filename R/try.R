input <- NULL

input$region <- "All"

input$net_type <- "Plant"
input$net_type <- "Pollinator"
input$name_type <- "Scientific names"
input$plants <- c("Abelia")
input$plants <- c("Abelia", "Mahonia\naquifolium", "Collinsia\nparviflora")
 
input$maximizer <-  "Pollinator abundance"
input$maximizer <-  "Phenological Coverage"
 
 input$n_plants <- 10
 
 input$native <- c("Native")

input$name_type <- "Scientific names"
input$bees <- c("Adela\nseptentrionella")

input$name_type <- "Common names"
input$bees <- c("Digger bees", "Mining bee", "Mason bees")
input$bees <- c("Digger bees")
sort(unique(db$bee_sp))

input$crop <- "Blueberry"

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
