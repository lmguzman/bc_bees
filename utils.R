
## plot function
plot_function <- function(act_type){
  if(act_type == "Build Network"){
    
    plot_gg()
    
  }else if(act_type == "Get plants"){
    maxi_plants()
  }else if(act_type == "Support crop"){
    plot_crop()
  }
}

## feedback form

questions <- list(
  list(id = "name", type = "text", title = "Name", mandatory = TRUE),
  list(id = "email", type = "text", title = "Email", mandatory = TRUE),
  list(id = "feedback", type = "text", title = "Feedback", mandatory = TRUE)
)
formInfo <- list(
  id = "basicinfo",
  questions = questions,
  storage = list(
    # Right now, only flat file storage is supported
    type = STORAGE_TYPES$FLATFILE,
    # The path where responses are stored
    path = "response"
  )
)