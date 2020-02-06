input <- NULL

input$region <- "Cowichan"

input$bees <- c("Bombus\nmixtus")

input$net_type <- "Plant"

input$plants <- c("Camassia\nquamash")


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
