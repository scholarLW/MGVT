trans.colors <- function(color,fill=FALSE,alpha=0){
  n = length(color)
  for(i in 1:n){
    gcolor = color[i]
    if(grepl(',',gcolor)){
      R = as.numeric(strsplit(gcolor,',')[[1]])[1]
      G = as.numeric(strsplit(gcolor,',')[[1]])[2]
      B = as.numeric(strsplit(gcolor,',')[[1]])[3]   
      if(fill){
        color[i] =  rgb(R,G,B,alpha,maxColorValue=255)  
      }else{
        color[i] =  rgb(R,G,B,255,maxColorValue=255) 
      }
	# 2019.06.25  
    }else{
	  require(grDevices)
	  gcolor = as.numeric(col2rgb(gcolor))
	  if(fill){
        color[i] =  rgb(gcolor[1],gcolor[2],gcolor[3],alpha,maxColorValue=255)  
      }else{
        color[i] =  rgb(gcolor[1],gcolor[2],gcolor[3],255,maxColorValue=255) 
      }
	}
  }
  return(color)
}