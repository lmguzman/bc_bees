input <- NULL

input$region <- "All"

input$bees <- c("Bombus\nmixtus")

input$net_type <- "Plant"

input$plants <- c("Camassia\nquamash")

input$maximizer <-  "Pollinator abundance"

input$n_plants <- 10

input$native <- c("Native")

sort(unique(db$bee_sp))
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
bip_table <- data.frame(table(db[,c("bee_sp", "plant_sp")])) %>% 
  spread(key = bee_sp, value = Freq) %>% 
  tibble::column_to_rownames("plant_sp")

#################################################################################
#Tylers stuff
#apply filter to db
get_mat <- function(df) {
  contin_tab <- as.data.frame.matrix(xtabs(~ plant_sp + bee_sp, df))
  return(contin_tab)
}

#filter the right data and convert to matrix
bip_table <- db %>%
  filter(plant_order == "Liliales") %>%
  get_mat() %>%
  rownames_to_column("plant_sp") %>%
  naniar::replace_with_na_all(condition = ~.x == 1) %>% 
  replace(is.na(.), 0) %>% 
  tibble::column_to_rownames("plant_sp") %>% View()

#################################################################################
#drop the ones from the filter table
bip_table <- bip_table %>% 
  rownames_to_column("plant_sp") %>%
  naniar::replace_with_na_all(condition = ~.x == 1) %>% 
  replace(is.na(.), 0) %>% 
  tibble::column_to_rownames("plant_sp") 

#remove cols with no values
bip_table <- bip_table[, -(which(colSums(bip_table)==0))]

#create network object
net <- bip_table %>% 
  network(matrix.type = "bipartite", 
          ignore.eval = FALSE, 
          names.eval = "weights")

#set attributes
#get plant native column for attributes
plant_att <- bip_table %>% 
  rownames_to_column("plant_sp") %>%
  left_join(db[,c("plant_sp", "plant_native", "plant_life_form")]) %>% 
  unique() %>% 
  dplyr::mutate(plant_native = capitalize(plant_native))

insect_att <- bip_table %>% 
  rownames_to_column("plant_sp") %>%
  pivot_longer(-plant_sp, "bee_sp", values_to = "count") %>% 
  pivot_wider(names_from = plant_sp, values_from = count) %>% 
  left_join(db[,c("bee_sp", "bee_diet", "bee_nest_location", "bee_guild")]) %>%
  unique() %>% 
  dplyr::mutate(bee_diet = capitalize(bee_diet))

#Set colour of nodes based on Phono
col <- c("Native"= "#18b583", "Non-native"="#f4a582", "Insect"="#1092de")
net %v% "phono" = c(plant_att$plant_native, rep("Insect", ncol(bip_table)))
phono = c(plant_att$plant_native, rep("Insect", ncol(bip_table)))
alp <- c("Pollenivore"= 1, "Herbivore"=1, "Predator"=1, "Parasite"= 1, "Detritivore"=1,"Herb" = 1)
net %v% "life" = c(plant_att$plant_life_form, insect_att$bee_diet)

#set edge attributes size and colour
set.edge.attribute(net, "eSize", log(net %e% "weights"))
#set.edge.attribute(net, "eSize", sqrt(net %e% "weights"))
set.edge.attribute(net, "eColor", ifelse(net %e% "weights" > 80, "#525252", 
                                         ifelse(net %e% "weights" < 3, "#d9d9d9", 
                                                ifelse(net %e% "weights" > 30, "#737373", "#969696"))))


#make plot
mynet <-ggnet2(net,
       label = FALSE,
       mode = "kamadakawai",
       #alpha = "life",
       #alpha.palette = alp,
       #alpha.legend = element_blank(),
       color = "phono",
       color.legend = "Group",
       edge.size = "eSize",
       edge.color = "eColor",
       palette = col,
       size = 5) +
  geom_point(aes(color = color), size = 12, color = "white") +
  geom_point(aes(color = color), size = 12, alpha = .75) +
  geom_point_interactive(aes(color = color, tooltip = gg$alpha, data_id = gg$alpha), size = 10) +
  geom_text(aes(label = label), color = "black", size = 1.25)

  girafe(ggobj = mynet, width_svg = 9, height_svg = 7)
  

install.packages("ggiraph")
library(ggiraph)






