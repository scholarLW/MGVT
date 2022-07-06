minmax <-function(x){
  return(x/sum(x))
}

corr.plot = function(x, matrix.transpose = FALSE, cor.method = c("pearson", "kendall", "spearman"), method = c("circle", "square", "ellipse", "number", "shade", "color", "pie"), type = c("full", "lower", "upper"), 
                     order = c("original","AOE", "FPC", "hclust", "alphabet"), hclust.method = c("complete", "ward", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
                     col = NULL, tl.cex = 0.8, tl.col = "black", addshade = c("negative", "positive", "all"), shade.lwd = 0.1, shade.col = NA, ...){
  # ... parameters of corrplot.
  # x: a file or matrix
  if(!(is.matrix(x) || is.array(x) || is.data.frame(x))){
    require(data.table)
    x = as.data.frame(fread(x,header = TRUE,sep="\t"))
    row.names(x) = as.character(x[,1])
    x = x[,-1]
  }
  if(matrix.transpose){
    x = t(x)
  }
  tmp = as.matrix(x) 
  tmp = t(apply(tmp,1,minmax))
  rownames(tmp) = rownames(x)  
  require(corrplot)
  if(is.null(col)){
    col = colorRampPalette(c('#00016A','#0200E5','#0337FD','#15B4FF','white','#FCD775','#FC8C3D','#E4161A','#800027'))(200)
  }
  if(length(cor.method)>1){
    cor.method = "pearson"
  }
  if(length(method)>1){
    method = "shade"
  }  
  if(length(type)>1){
    type = "upper"
  }   
  if(length(order)>1){
    order = "hclust"
  } 
  if(length(hclust.method)>1){
    hclust.method = "ward.D"
  }  
  if(length(addshade)>1){
    addshade = "positive"
  }    
  
  M = cor(tmp, method=cor.method) 
  corrplot(corr=M,method=method,type=type,order=order,hclust.method=hclust.method,col=col,tl.cex=tl.cex,tl.col=tl.col,addshade=addshade,shade.lwd=shade.lwd,shade.col=shade.col,...) 
}