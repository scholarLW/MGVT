arrowsplot <- function(x, y, col, lwd, arr.length, lty, distance, arr.gap.error){
  x1 = x[-length(x)]
  x2 = x[-1]
  y1 = y[-length(y)]
  y2 = y[-1]           
  for(j in 1:length(y1)){
    cpoints = dc.centerMin(c(x1[j],y1[j]),c(x2[j],y2[j]),distance,arr.gap.error)
    Arrows(x1[j],y1[j],cpoints[1],cpoints[2],col=col,bg=col,lwd=lwd,arr.length=arr.length,lty=lty)    
  }   
}

arrows <- function(x, y=NULL, group=NULL, col=NULL, lwd=1, arr.length=0.2, lty=1, distance=10^-3, arr.gap.error=10^-2,
                   plot=FALSE, plot.points=plot, points.pch=NULL, points.col=col, points.bg=points.col, points.cex=1,
                   plot.text=plot, text.adj = 0, text.cex = 1, text.col = col, text.font = NULL, ...){  
  
  # ... further plot parameters.  
  require(shape) 
  require(Funset)
  if(!(is.matrix(x) || is.array(x) || is.data.frame(x)) && !(is.vector(x) && is.vector(y) && length(x) == length(y))){
    return("Error inputdata!") 
    stop()	
  }
  if(is.null(y)){
    if(is.matrix(x) || is.array(x) || is.data.frame(x) && ncol(x) == 2) {
      y <- x[,2]
      x <- x[,1]
    }else{
        return("x and y must be vectors, or x must be a 2 column matrix!")
		stop()
    }
  }else
    if(!(is.vector(x) && is.vector(y) && length(x) == length(y))){   
      return("x and y must be vectors of the same length")  
	  stop()
    }
  
  ng = 1
  ngg = 'g1'
  if(!is.null(group)){    
    ngg = sort(as.character(unique(group)))
    ng = length(ngg) # group is a vector with the same length of x
    if(length(group) != length(x)){
      return("x and group must be vectors of the same length")
      stop()	  
    }
  }else{
    group = rep(ngg,length(x))
  }  
  if(is.null(col)){    
    col = Colorpools()[1:ng]
  }
  if(is.null(points.col)){
    points.col = col
  }
  if(is.null(text.col)){
    text.col = col
  }  
  if(is.null(points.pch)){
    points.pch = Grouppointpools()[1:ng]
  } 
  if(is.null(points.bg)){
    points.bg = points.col
  } 
  if(length(col)){
    col = rep(col,ng)[1:ng]
  }  
  if(length(points.col)){
    points.col = rep(points.col,ng)[1:ng]
  }
  if(length(points.bg)){
    text.col = rep(points.bg,ng)[1:ng]
  }  
  if(length(text.col)){
    text.col = rep(text.col,ng)[1:ng]
  }   
  if(length(points.pch)){
    points.pch = rep(points.pch,ng)[1:ng]
  }    
  if(plot){
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(8,4), heights=c(8))
    #layout.show(2)
    # 1 #
    par(mar = c(5,5,5,2))
    plot(0, 0, type='l', xlim = c(min(x),max(x)), ylim = c(min(y),max(y)), ...)
    for(i in 1:ng){
      ix = group == ngg[i]
      if(sum(ix)>0){
        xg = x[ix]
        yg = y[ix]
        colg = col[i]
        arrowsplot(xg, yg, colg, lwd, arr.length, lty, distance, arr.gap.error) 
      }
    }

    if(plot.points){
      for(i in 1:ng){
        ix = group == ngg[i]
        if(sum(ix)>0){
          xg = x[ix]
          yg = y[ix]
          pchg = points.pch[i]
		# 2019.07.23
		#################
	    if(!is.na(as.numeric(pchg))){
           pchg = as.numeric(pchg)
        }		
		################# 
          bgg = points.bg[i]
          colg = points.col[i]          
          points(xg, yg, pch=pchg, bg=bgg, col=colg, cex=points.cex) 
        }
      } 
    }
    # 2 #
    par(mar = c(5,0,5,5))
    plot(0, 0, type='l', xlim = c(0,1), ylim = c(0,ng+1), xlab='', ylab='', axes = FALSE)   
    if(plot.text){
      for(i in 1:ng){
        y1 = (i-1)*1
        arrowsplot(c(0,0.15), c(y1,y1), col[i], lwd=2, arr.length, lty, distance, arr.gap.error)
		# 2019.07.23
		#################
		pchg = points.pch[i]
	    if(!is.na(as.numeric(pchg))){
           pchg = as.numeric(pchg)
        }	
		points(0.25, y1, pch=pchg, bg=points.bg[i], col=points.col[i], cex=points.cex)		
		################# 		
        #points(0.25, y1, pch=points.pch[i], bg=points.bg[i], col=points.col[i], cex=points.cex) # 2019.07.23
        text(0.35, y1,labels=ngg[i],col=text.col[i],adj=text.adj, cex=text.cex, font=text.font)   
      } 
    }
  }else{
    for(i in 1:ng){
      ix = group == ngg[i]
      if(sum(ix)>0){
        xg = x[ix]
        yg = y[ix]
        colg = col[i]
        arrowsplot(xg, yg, colg, lwd, arr.length, lty, distance, arr.gap.error) 
      }
    }
  } 
}
