dataPC.plot <- function(points=NULL, line=FALSE, ellipse=FALSE, fill=FALSE, ellipse.method=MethodList(), 
                          col=NULL, lwd=2, lty=1, border=NA, alpha=150, axis.scale=NULL, ayis.scale=NULL,  
                          points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=1, 
                          text.adj = 0, text.cex = 1, text.col = NULL, text.font = NULL, 
                          axes=TRUE, ...){
  
  # ... further plot parameters.
  nameID = colnames(points)
  if(! 'SampleID' %in% nameID){
    return("Please enter columns 'SampleID' for points!")
	stop()
  }
  if(! 'HostID' %in% nameID){
    return("Please enter columns 'HostID' for points!")
	stop()
  }   
  arrow = 0
  if("Timeseries" %in% nameID){
    arrow = 1
    points = points[order(as.numeric(points$Timeseries)),]    
  }  
  ng = 1
  ngg = NULL   
  if(!"Group" %in% nameID){
    points$Group = as.character(points$HostID)
  }
  ngg = sort(unique(as.character(points$Group)))
  ng = length(ngg) 
  
  if(is.null(col)){
    if("Groupcolor" %in% nameID){
      col = unique(as.character(points$Groupcolor))
    }else{                
      if(fill){
        col = gpColorpools()[1:ng]
      }else{
        col = Colorpools()[1:ng]  
      }
      if(ng>=6){
        col = '211,232,246'
      }
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
  if(length(ellipse.method)>1){
    ellipse.method = 'Funset'
    if(ng == 1){
      ellipse.method = 'car'
    }
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
  points$x = as.numeric(points$x)
  points$y = as.numeric(points$y)
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
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(8,8), heights=c(8))
  #layout.show(2)
  # 1 #
  par(mar = c(5,5,5,0.5))
  if(axes){
    plot(0, 0, type='l', axes=TRUE, xlim = c(-xnl*axis.scale,xnr*axis.scale), ylim = c(-ynl*ayis.scale,ynr*ayis.scale), ...)
  }else{
    plot(0, 0, type='l', axes=FALSE, xlim = c(-xnl*axis.scale,xnr*axis.scale), ylim = c(-ynl*ayis.scale,ynr*ayis.scale), ...)
    axis(1,at=seq(-xnl*axis.scale,xnr*axis.scale,axis.scale),labels=seq(-xnl*axis.scale,xnr*axis.scale,axis.scale),col="black",col.axis="black",lwd=lwd)
    axis(2,at=seq(-ynl*ayis.scale,ynr*ayis.scale,ayis.scale),labels=seq(-ynl*ayis.scale,ynr*ayis.scale,ayis.scale),col="black",col.axis="black",lwd=lwd)
  }
 
  if(ellipse){
    for(i in 1:ng){
      ix = points$Group == ngg[i]
      if(sum(ix)>0){
        xg = points$x[ix]
        yg = points$y[ix]
        group = as.character(points$HostID[ix])
        colg = col[i]        
        ellipse(xg, yg, group=group, method=ellipse.method, col=colg, fill=fill, border=border, alpha=alpha, lty=lty)
      }
    }
  } 
  # points
	#group.mark <- p.bg <- p.col <- NULL # 2019.07.23
	p.col <- NULL # 2019.07.23
	for(i in 1:nrow(points)){
	    group.mark <- p.bg <- p.col1 <- NULL # 2019.07.23
		ix = ngg == points$Group[i]
		group.mark = c(group.mark,points.pch[ix])
		p.bg = c(p.bg,points.bg[ix])
		p.col = c(p.col,points.col[ix])
		# 2019.07.23
		#################
		p.col1 = c(p.col1,points.col[ix])
		if(!is.na(as.numeric(group.mark))){
           group.mark = as.numeric(group.mark)
        }	  
		points(points$x[i],points$y[i],pch=group.mark,bg=p.bg,col=p.col1,cex=points.cex) # 2019.07.23
		####################
	}
	#points(points$x,points$y,pch=group.mark,bg=p.bg,col=p.col,cex=points.cex) # 2019.07.23 
	
  if(line){   
    for(i in 1:ng){
      if(arrow == 1){
        ix = points$Group == ngg[i]
        if(sum(ix)>0){
          xg = points$x[ix]
          yg = points$y[ix]
          group = as.character(points$HostID[ix])		  
          colg = p.col[ix]        
          arrows(x=xg, y=yg, group=group, col=colg, lwd=lwd)          
        } 
      }else{
        ix = points$Group == ngg[i]
        if(sum(ix)>1){
          x = points$x[ix]
          y = points$y[ix]
          x1 = x[-length(x)]
          x2 = x[-1]
          y1 = y[-length(y)]
          y2 = y[-1]    
          segments(x1,y1,x2,y2,col=col[i],bg=col[i], lwd=lwd)
        }
      }
    }  
  }
  
  # 2 #
#  par(mar = c(5,0,5,5))
#  plot(0, 0, type='l', xlim = c(0,1), ylim = c(0,ng+1), xlab='', ylab='', axes = FALSE)   
#  if(length(unique(col))==1 && length(unique(points.col))==1){  # 2019.07.24
#  #if(length(unique(col))==1)){
#    ng = 1
#    ngg = 'Person ellipse'
#  }  
  #for(i in 1:ng){
  #  y1 = i*0.5
  #  xg = c(0,0.25,0.1)
  #  yg = c(y1,y1,y1+0.1)
  #  colg = col[i]
   # if(ellipse){
   #   ellipsedata <- MethodLoad('Funset',xg,yg)
   #   elli.plot(ellipsedata,fill=fill,lwd=lwd,lty=lty,col=colg,border=border)
  #  }
	# 2019.07.23
	#################	
 #   a =  points.pch[i]
#	if(!is.na(as.numeric(a))){
 #      a = as.numeric(a)
 #   }	
 #   points(0.1, y1+0.05, pch=a, bg=points.bg[i], col=points.col[i], cex=0.8)
 #   if(line){
 #     if(arrow == 1){
#	    mgg = yg[1:2] + 0.05
  #      arrows(x=c(0.05,0.15), y=mgg, col=points.col[i], lwd=0.5, arr.length=0.1, distance=10^-3, arr.gap.error=10^-3)  
  #    }else{
  #      segments(0.05, yg[1], 0.15, yg[1], col=points.col[i], bg=points.col[i], lwd=0.5)
  #    }  
  #  }
  #  text(0.35, y1,labels=ngg[i],col=text.col[i],adj=text.adj, cex=text.cex, font=text.font)       
  #}
  #if(length(unique(points.pch))==1){
  #  points(0.1, 0, pch=points.pch[1], bg=NA, col='black', cex=1.1)
  #  text(0.5,0,labels ="Microbiome sample")	  
  #}else{
  #  #text(0.1,y1+1,labels='Group',col='#7F7F7F',adj=0, cex=1.2, font=2)  
#	text(0,2.5,labels='Group',col='#7F7F7F',adj=0, cex=1.05, font=1)
 # }	   
 par(mar = c(5,0,5,5)) 
 if(length(unique(col))==1 && length(unique(points.col))==1){  # 2019.07.24
    ng = 1
    ngg = 'Person ellipse'
  }   
  xn = ceiling(ng/4)
  plot(0, 0, type='l', xlim = c(-0.1,xn+0.35), ylim = c(0,10), xlab='', ylab='', axes = FALSE)  
  for(i in 1:xn){
    for(j in 1:4){
      if(j+4*(i-1)<=ng){
        xg = c(0,0.2,0.1,0.1)+1*(i-1)
        yg = c(j*0.5,j*0.5,j*0.5+0.2,j*0.5-0.2)	  
        if(ellipse){ 
          #points(0.05+1*(i-1),j*0.5,col=col[j+4*(i-1)],bg=col[j+4*(i-1)],pch=22,cex=2.5)
          colg = col[j+4*(i-1)]
          ellipsedata <- MethodLoad('Funset',xg,yg)
          elli.plot(ellipsedata,fill=fill,lwd=lwd,lty=lty,col=colg,border=border) 		  		  
        }else{
	      points(0.05+1*(i-1),j*0.5,col=points.col[j+4*(i-1)],bg='grey100',pch=22,cex=2.5)
	    }	  
        a = points.pch[j+4*(i-1)]
		if(!is.na(as.numeric(a))){
           a = as.numeric(a)
        }		  		
        points(0.05+1*(i-1),j*0.5,col=points.col[j+4*(i-1)],bg=points.bg[j+4*(i-1)],pch=a,cex=1.2)	
        text(0.25+1*(i-1),j*0.5,labels=ngg[j+4*(i-1)],col=text.col[i],adj=text.adj, cex=text.cex, font=text.font)  
        if(line){
          if(arrow == 1){
              arrows(x=c(0.05,0.15)+1*(i-1), y=c(j*0.5,j*0.5), col=points.col[j+4*(i-1)], lwd=0.5, arr.length=0.1, distance=10^-3, arr.gap.error=10^-3)  
          }else{
              segments(0.05+1*(i-1), j*0.5, 0.15+1*(i-1), j*0.5, col=points.col[j+4*(i-1)], bg=points.col[j+4*(i-1)], lwd=0.5)
          }  
        }		
      }     
    }        
  }
 
  if(length(unique(points.pch))==1){
    points(0.1, 0, pch=points.pch[1], bg=NA, col='black', cex=1.1)
    text(0.5,0,labels ="Microbiome sample")	  
  }else{
	text(0,2.5,labels='Group',col='#7F7F7F',adj=0, cex=1.05, font=1)
  }	  
}   