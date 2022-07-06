Rarefaction <- function(file, matrix.transpose=TRUE, method='random', permutations=100, conditioned=TRUE, box.col='yellow', outliers.pch="+",       # box 
                          plot.col="blue", ci.type="poly", lwd=2, ci.lty=0, ci.col="lightblue", ci.alpha=150, ...){           # plot 
  if(is.null(file)){
    return("Please enter abundance file!")
	stop()
  }else{ 
      if(!(is.matrix(file) || is.array(file) || is.data.frame(file))){
	    require(data.table)
        da = as.data.frame(fread(file)) 
        rownames(da) = da[,1]
        da = da[,-1]
      }else{
        da = as.data.frame(file)
      }
    }
  
   # ... further plot parameters.
   
   # Data processing ...
   require(vegan)   
   # keep rownames were the samples names
   if(isTRUE(matrix.transpose)){
      da = as.data.frame(t(da))
   } 
   ci.col = trans.colors(ci.col,TRUE,ci.alpha)
   sp1 <- specaccum(da, method,permutations,conditioned)  
   plot(sp1, ci.type=ci.type, col=plot.col, lwd=lwd, ci.lty=ci.lty, ci.col=ci.col, ...)
   boxplot(sp1, col=box.col, add=TRUE, pch=outliers.pch)
}
