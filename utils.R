

plot_function <- function(act_type){
  if(act_type == "Build Network"){
    
    plot_gg()
    
  }else if(act_type == "Get plants"){
    maxi_plants()
  }else if(act_type == "Support crop"){
    plot_crop()
  }
}