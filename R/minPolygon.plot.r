minPolygon.plot <- function(points=NULL, fill=FALSE, col=NULL, lwd=2, lty=1, border=NA, alpha=150, plot=TRUE, addPoints=TRUE,
                            axis.scale=NULL, ayis.scale=NULL, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=1, 
                            text.adj = 0, text.cex = 1, text.col = NULL, text.font = NULL, axes=TRUE, ...){
  
  # ... further plot parameters.
  require(Funset)
  nameID = colnames(points)
  ng = 1
  ngg = NULL   
  if(!"Group" %in% nameID){
   return("Please enter column 'Group' for matrix points!")
   stop()
  }
  ngg = sort(unique(as.character(points$Group)))
  ng = length(ngg) 
  
  if(is.null(col)){               
      if(fill){
        col = gpColorpools()[1:ng]
      }else{
        col = Colorpools()[1:ng]  
      }
  } 
  if(is.null(points.col)){
    points.col = Colorpools()[1:ng]
  } 
  if(is.null(points.pch)){
    points.pch = Grouppointpools()[1:ng]  
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
  if(is.null(axis.scale)){
    axis.scale = 0.2      
  }
  if(is.null(ayis.scale)){
    ayis.scale = 0.2      
  }     
  # scale
  points$x = as.numeric(as.character(points$x))
  points$y = as.numeric(as.character(points$y))
  if(sum(points$x<0)>0){
    xmax_l = max(abs(points$x[points$x<0]))
  }else{
    xmax_l = 0
  }
  if(sum(points$x>0)>0){
    xmax_r = max(abs(points$x[points$x>0]))
  }else{
    xmax_r = 0
  }
  if(sum(points$y<0)>0){
    ymax_l = max(abs(points$y[points$y<0]))
  }else{
    ymax_l = 0
  }
  if(sum(points$y>0)>0){
    ymax_r = max(abs(points$y[points$y>0]))
  }else{
    ymax_r = 0
  }
  xnl = ceiling(xmax_l/axis.scale)
  xnr = ceiling(xmax_r/axis.scale)
  ynl = ceiling(ymax_l/ayis.scale)
  ynr = ceiling(ymax_r/ayis.scale)
  
  if(plot){
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(8,4), heights=c(8))
  #layout.show(2)
  # 1 #
  par(mar = c(5,5,5,2))
  if(axes){
    plot(0, 0, type='l', axes=TRUE, xlim = c(-xnl*axis.scale,xnr*axis.scale), ylim = c(-ynl*ayis.scale,ynr*ayis.scale), ...)
  }else{
    plot(0, 0, type='l', axes=FALSE, xlim = c(-xnl*axis.scale,xnr*axis.scale), ylim = c(-ynl*ayis.scale,ynr*ayis.scale), ...)
    axis(1,at=seq(-xnl*axis.scale,xnr*axis.scale,axis.scale),labels=seq(-xnl*axis.scale,xnr*axis.scale,axis.scale),col="black",col.axis="black",lwd=lwd)
    axis(2,at=seq(-ynl*ayis.scale,ynr*ayis.scale,ayis.scale),labels=seq(-ynl*ayis.scale,ynr*ayis.scale,ayis.scale),col="black",col.axis="black",lwd=lwd)
  }
  }
  
  for(i in 1:ng){    
    ix = points$Group == ngg[i]
    if(sum(ix)>2){
      xg = points$x[ix]
      yg = points$y[ix]
      colg = col[i]  
      da = as.data.frame(cbind(xg,yg))
      colnames(da)=c('x','y')
      da = Polygon(da)
      da = rbind(da,da[1,])
      if(fill){    
        polygon(da, col=colg, border=border)
      }else{
        lines(da, col=colg, lwd=lwd, lty=lty)
      } 
    }
    if(sum(ix)==2){
      xg = points$x[ix]
      yg = points$y[ix]
      colg = col[i]  
      segments(xg[1],yg[1],xg[2],yg[2],col=colg, lwd=lwd, lty=lty)
    }
  }
  
  if(addPoints){
	# points
	#group.mark <- p.bg <- p.col <- NULL # 2019.07.23
	for(i in 1:nrow(points)){
	    group.mark <- p.bg <- p.col <- NULL # 2019.07.23
		ix = ngg == points$Group[i]
		group.mark = c(group.mark,points.pch[ix])
		p.bg = c(p.bg,points.bg[ix])
		p.col = c(p.col,points.col[ix])
		# 2019.07.23
		#################
		if(!is.na(as.numeric(group.mark))){
           group.mark = as.numeric(group.mark)
        }	  
		points(points$x[i],points$y[i],pch=group.mark,bg=p.bg,col=p.col,cex=points.cex) # 2019.07.23
		####################
	}
	#points(points$x,points$y,pch=group.mark,bg=p.bg,col=p.col,cex=points.cex) # 2019.07.23     
  }

  if(plot){
    # 2 #
  par(mar = c(5,0,5,5))
  plot(0, 0, type='l', xlim = c(0,1), ylim = c(0,ng+1), xlab='', ylab='', axes = FALSE)   
  for(i in 1:ng){
    y1 = i*0.5
    colg = col[i]   
    points(0.1, y1, pch=15, bg=colg, col=colg, cex=2.5)
	#points(0.1, y1, pch=points.pch[i], bg=points.col[i], col=points.col[i], cex=points.cex)  # 2019.07.23
	# 2019.07.23
	#################	
    a =  points.pch[i]
	if(!is.na(as.numeric(a))){
       a = as.numeric(a)
    }
	points(0.1, y1, pch=a, bg=points.col[i], col=points.col[i], cex=points.cex)  # 2019.07.23
	##################
    text(0.35, y1,labels=ngg[i],col=text.col[i],adj=text.adj, cex=text.cex, font=text.font)       
  } 
  }
}   