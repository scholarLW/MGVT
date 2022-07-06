elli.plot <- function(ellipse,fill=TRUE,lwd=1,lty=1,col='red',border=NA){
  if(fill){    
    polygon(ellipse, col=col, border=border)
  }else{
    lines(ellipse, col=col, lwd=lwd, lty=lty)
  } 
}

ellipse <- function(x, y=NULL, group=NULL, method=c('Funset','car'), col=NULL, lwd=1, lty=1, fill=FALSE, border=NA, alpha=150,
                    plot=FALSE, axis.scale=NULL, ayis.scale=NULL, axes=TRUE, xlab='', ylab='',main='',
                    plot.points=plot, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=1,
                    plot.text=plot, text.adj = 0, text.cex = 1, text.col = NULL, text.font = NULL, ...){
  
  # ... further parameters of method to generate an ellipse from R package. 
  if(!(is.matrix(x) || is.array(x) || is.data.frame(x)) && !(is.vector(x) && is.vector(y) && length(x) == length(y))){
    return("Error inputdata!") 
    stop()	
  }
  if(is.null(y)){
    if(is.matrix(x) || is.array(x) || is.data.frame(x) && ncol(x) == 2) {
      y <- as.numeric(as.character(x[,2]))
      x <- as.numeric(as.character(x[,1]))
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
      return("x and group must be vectors of the same length!")
      stop()	  
    }
    if(length(method)>1){
      method = 'Funset'
    }
  }else{
    group = rep(ngg,length(x))
    method = 'car'
  }  
  if(is.null(col)){    
    if(fill){
	  col = gpColorpools()[1:ng]
	}else{
	  col = Colorpools()[1:ng]
	}
  }
  if(is.null(points.col)){
    points.col = Colorpools()[1:ng]
    if(fill){
      if(ng>=6){
        points.col = '#0088CE'
      }
    }
  } 
  if(is.null(points.pch)){
    points.pch = Grouppointpools()[1:ng]
    if(fill){
      if(ng>=6){
        points.pch = 20
      }
    }    
  }   
  if(is.null(points.bg)){
    points.bg = points.col
  } 
  if(is.null(text.col)){
    text.col = 'black'
  }  
  if(length(col)){
    col = rep(col,ng)[1:ng]
  }  
  if(length(points.col)){
    points.col = rep(points.col,ng)[1:ng]
  }
  if(length(points.bg)){
    points.bg = rep(points.bg,ng)[1:ng]
  }  
  if(length(text.col)){
    text.col = rep(text.col,ng)[1:ng]
  }   
  if(length(points.pch)){
    points.pch = rep(points.pch,ng)[1:ng]
  }
  
  if(fill){
    n = length(col)
    for(i in 1:n){
      gcolor = col[i]
      if(!grepl(',',gcolor)){
        data(colorpools)
        if(grepl('#',gcolor)){
          ix = color$n16 == gcolor
          if(sum(ix)>0){
            col[i] = color$nrgb[ix][1]
          }        
        }else
          if(! grepl(',',gcolor)){
            ix = color$nrgb == gcolor
            if(sum(ix)>0){
              col[i] = color$nrgb[ix][1]
            }                    
          }
      }
    }
  }
  col = trans.colors(col,fill,alpha)
  points.col = trans.colors(points.col,FALSE,0)
  points.bg = trans.colors(points.bg,FALSE,0)
  text.col = trans.colors(text.col,FALSE,0)
  if(isTRUE(plot)){
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(8,4), heights=c(8))
    #layout.show(2)
    # 1 #
    par(mar = c(5,5,5,2))
    
    if(is.null(axis.scale)){
      axis.scale = 0.2      
    }
    if(is.null(ayis.scale)){
      ayis.scale = 0.2      
    }    
    # scale
    if(sum(x<0)>0){
      xmax_l = max(abs(x[x<0]))
    }else{
      xmax_l = 0
    }
    if(sum(x>0)>0){
      xmax_r = max(abs(x[x>0]))
    }else{
      xmax_r = 0
    }
    if(sum(y<0)>0){
      ymax_l = max(abs(y[y<0]))
    }else{
      ymax_l = 0
    }
    if(sum(y>0)>0){
      ymax_r = max(abs(y[y>0]))
    }else{
      ymax_r = 0
    }
    xnl = ceiling(xmax_l/axis.scale)
    xnr = ceiling(xmax_r/axis.scale)
    ynl = ceiling(ymax_l/ayis.scale)
    ynr = ceiling(ymax_r/ayis.scale)
    if(axes){
      plot(0, 0, type='l', axes=TRUE, xlim = c(-xnl*axis.scale,xnr*axis.scale), ylim = c(-ynl*ayis.scale,ynr*ayis.scale), xlab = xlab, ylab = ylab, main = main)
    }else{
      plot(0, 0, type='l', axes=FALSE, xlim = c(-xnl*axis.scale,xnr*axis.scale), ylim = c(-ynl*ayis.scale,ynr*ayis.scale), xlab=xlab, ylab=ylab, main=main)
      axis(1,at=seq(-xnl*axis.scale,xnr*axis.scale,axis.scale),labels=seq(-xnl*axis.scale,xnr*axis.scale,axis.scale),col="black",col.axis="black",lwd=lwd)
      axis(2,at=seq(-ynl*ayis.scale,ynr*ayis.scale,ayis.scale),labels=seq(-ynl*ayis.scale,ynr*ayis.scale,ayis.scale),col="black",col.axis="black",lwd=lwd)
    }

    for(i in 1:ng){
      ix = group == ngg[i]
      if(sum(ix)>0){
        xg = x[ix]
        yg = y[ix]
        colg = col[i]        
        ellipse <- MethodLoad(method,xg,yg,...)
        elli.plot(ellipse,fill=fill,lwd=lwd,lty=lty,col=colg,border=border)
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
        y1 = i*0.5
        xg = c(0,0.25)
        yg = c(y1,y1)
        colg = col[i]
        ellipse <- MethodLoad('Funset',xg,yg)
        elli.plot(ellipse,fill=fill,lwd=lwd,lty=lty,col=colg,border=border)
        if(plot.points){
		   a = points.pch[i]
		   # 2019.07.23
		   #################
		   if(!is.na(as.numeric(a))){
		      a = as.numeric(a)
		   }			  
		   points(0.1, y1, pch=a, bg=points.bg[i], col=points.col[i], cex=0.8)
        }
        text(0.35, y1,labels=ngg[i],col=text.col[i],adj=text.adj, cex=text.cex, font=text.font)   		
      }
	  if(length(unique(points.pch))==1){
	    points(0.1, 0, pch=points.pch[1], bg=NA, col='black', cex=1.1)
        text(0.5,0,labels ="Microbiome sample")	  
	  }	  
    }
  }else{
    for(i in 1:ng){
      ix = group == ngg[i]
      if(sum(ix)>0){
        xg = x[ix]
        yg = y[ix]
        colg = col[i]        
        ellipse <- MethodLoad(method,xg,yg,...)
        elli.plot(ellipse,fill=fill,lwd=lwd,lty=lty,col=colg,border=border)
      }
    }
  }  
}
