#functions used 

shift_axis <- function(p, y=0){
  g <- ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), 
                        ymax=y, ymin=y) +
    geom_hline(aes(yintercept=y), data = dummy) +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x=element_blank())
  
}

#get percentage
get_percentage<- function(data,var){
  
  freq = table(data[,var])
  labels = names(freq)
  values = as.vector(freq)
  prop =  as.vector(freq/length(data[,var]))
  perc = round(prop,2)*100
  
  perc_data<- data.frame(labels,values,prop,perc)
  return(perc_data)
}

#get mode
get_mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


#Works but not needed
bootstrap_percentage<- function(data,i,var){
  
  if(!is.numeric(data[,var])){
    data[,var]<- as.numeric(data[,var])
  }
  
  df<-data[i,]
  tb<- table(data[,var])/length(data[,var])
  return(tb)
}


#LOL, there is a native R implementation XD 
# 
# some data
# m <- matrix(1:4, 2)
# calculate proportions
# proportions(m, 1)
# or do percentages as such
# DF <- as.data.frame(UCBAdmissions)
# tbl <- xtabs(Freq ~ Gender+Freq, DF)
# proportions(tbl, `Gender`)