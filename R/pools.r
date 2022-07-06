Colorpools <- function(){ 
  data(colorpools) 
  Colorpools = c("#0088CE","#BD0000", "#FFD91B",'#3CB420','#B848FF') 
  Colorpools = unique(c(Colorpools,color$n16))
  return(Colorpools)
}
gpColorpools <- function(){
  data(colorpools)
  gpColorpools = c('211,232,246','246,213,213','255,247,220','225,240,218','221,160,221')
  gpColorpools = unique(c(gpColorpools,color$nrgb))
  return(gpColorpools)
}
Grouppointpools <- function(){
  Grouppointpools = c(21:25,1:14)
  return(Grouppointpools)
}  