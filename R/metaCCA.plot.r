metaCCA.plot = function(x, group=NULL, addEllipse=TRUE, addPolygon=!addEllipse, fill=TRUE, ellipse.method='car', car.level=0.95, col=NULL, lty=1, border=NA, alpha=150, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=1){
  
  # x CCA objects 
  if(all(colnames(t(lengths(x))) %in% c("colsum","tot.chi","Ybar","method","call","pCCA", "CCA","CA","inertia","regularization","terms","terminfo"))){
    method = toupper(x$method)
    sp1 = x
    ef = NULL   
	if(is.null(group)){
	  return("Group must be set for a CCA objects inputdata!")
	  stop()
	}
	group = as.character(group)
  }else{
    method = toupper(x$sp1$method)
    sp1 = x$sp1
    ef = x$ef # NULL
  if(is.null(group)){
      group = as.character(x$data$Group)
  }
  }
  vectors = NULL
  ii <- summary(sp1)  
  st <- as.data.frame(ii$constraints[,1:2])   # pca axis
  #sp_pre <- as.data.frame(ii$species[,1:2])   # species axis
  cons = sp1$CCA$eig/sum(sp1$CCA$eig) # contribution
  if(!is.null(ef)){
    vectors <- ef$vectors  # vectors
  }else{
    vectors <- ii$biplot
  }
  if(addPolygon){
    addEllipse=!addPolygon
  }
  if(is.list(vectors)){
    x = c(st[,1],as.numeric(vectors$arrows[,1]))   
    y = c(st[,2],as.numeric(vectors$arrows[,2]))   
  }else{
    x = c(st[,1],vectors[,1]) 
    y = c(st[,2],vectors[,2]) 	
  } 

  if(!is.vector(group)){
    return("Group must be a vector!")
	stop()
  }else{
    if(length(group) != nrow(st)){
      return("The length of group must match the number of samples which use to analyze!")
	  stop()
    }
  }
  ngg = sort(unique(as.character(group)))
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
  if(length(col)){
    col = rep(col,ng)[1:ng]
  }  
  if(length(points.col)){
    points.col = rep(points.col,ng)[1:ng]
  }
  if(length(points.bg)){
    points.bg = rep(points.bg,ng)[1:ng]
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
  xmax = ceiling(max(abs(x)))
  ymax = ceiling(max(abs(y)))
  axis.scale =  xmax/5
  ayis.scale = ymax/5
  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(10,5), heights=c(2,8))
  #layout.show(2)
  # 1 #
  par(mar = c(5,5,5,1))
  plot(0, 0, type='l', axes=FALSE, xlim = c(-xmax-axis.scale,xmax+1.5*axis.scale), ylim = c(-ymax-ayis.scale,ymax+ayis.scale), xlab='',ylab='',main='')     
  if(addEllipse){
    if(ellipse.method %in% 'car'){
	      MGVT::ellipse(st,group=group,method=ellipse.method,col=col,lty=lty,fill=fill,border=border,alpha=alpha,level=car.level)
	}else{
	      MGVT::ellipse(st,group=group,method=ellipse.method,col=col,lty=lty,fill=fill,border=border,alpha=alpha) 
	}
  }	
  if(addPolygon){
    points = cbind(rownames(st),group,st)
    colnames(points) = c('SampleID','Group','x','y')
    minPolygon.plot(points, fill=fill, col=col, lty=lty, border=border, alpha=alpha, plot=FALSE, points.pch=points.pch, points.col=points.col, points.bg=points.bg, points.cex=points.cex)
  }
  if(!addPolygon){
    # points
    #group.mark <- p.bg <- p.col <- NULL  # 2019.07.23
    for(i in 1:nrow(st)){
	  group.mark <- p.bg <- p.col <- NULL # 2019.07.23
      ix = ngg == group[i]
      group.mark = c(group.mark,points.pch[ix])
      p.bg = c(p.bg,points.bg[ix])
      p.col = c(p.col,points.col[ix])
	  # 2019.07.23
	  #################
		if(!is.na(as.numeric(group.mark))){
           group.mark = as.numeric(group.mark)
        }	 	  
	  points(as.numeric(as.character(st[i,1])),as.numeric(as.character(st[i,2])),pch=group.mark,bg=p.bg,col=p.col,cex=points.cex)
	  #################	  
    }
    #points(as.numeric(as.character(st[,1])),as.numeric(as.character(st[,2])),pch=group.mark,bg=p.bg,col=p.col,cex=points.cex) # 2019.07.23	
  }
  segments(0,-ymax,0,ymax,lty=2,lwd=0.8)
  segments(-xmax,0,xmax,0,lty=2,lwd=0.8)	
  x = seq(-xmax,xmax,axis.scale)
  y = seq(-ymax,ymax,ayis.scale)
  segments(-xmax-axis.scale/9,y[-length(y)][-1],-xmax,y[-length(y)][-1],lty=1,lwd=1,col=trans.colors('grey40',TRUE,200))
  segments(x[-length(x)][-1],-ymax-ayis.scale/6.5,x[-length(x)][-1],-ymax,lty=1,lwd=1,col=trans.colors('grey40',TRUE,200))
  segments(-xmax,y[-length(y)],xmax,y[-length(y)],lty=1,lwd=2,col=trans.colors('grey90',TRUE,60))
  segments(x[-length(x)],-ymax,x[-length(x)],ymax,lty=1,lwd=2,col=trans.colors('grey90',TRUE,60))		
  text(x[-length(x)][-1],-ymax-ayis.scale/2,labels=x[-length(x)][-1],col='grey40',cex=0.8)
  text(-xmax-axis.scale/1.7,y[-length(y)][-1],labels=y[-length(y)][-1],col='grey40',cex=0.8)	
  mtext(paste(method,"2 (",round(cons[2],2)*100,"%)",sep=""),side=2,col='grey40',cex=1,las=3)	
  mtext(paste(method,"1 (",round(cons[1],2)*100,"%)",sep=""),side=1,col='grey40',cex=1)	
  mtext(paste(method,"plot",sep=""),side=3,col='grey',cex=1.5,adj=0.5,padj=0)
  
  if(is.list(vectors)){
  name = as.character(rownames(vectors$arrows))
  R2 = as.numeric(vectors$r)
  R2 = round(R2,2)*100
  #mycolors = trans.colors(colorRampPalette(c('yellow','orange','red','red4'))(100),TRUE,80)
  mycolors = trans.colors(colorRampPalette(c('#FFFDCB','#FFEB9B','#FCD775','#FDB04D','#FC8C3D','#FB4D2A','#E4161A','#BC0026','#800027'))(100),TRUE,150)
  pvalue = as.numeric(vectors$pvals)
  xy = cbind(as.numeric(vectors$arrows[,1]),as.numeric(vectors$arrows[,2]))
  }else{
    name = as.character(rownames(vectors))
    xy = cbind(as.numeric(vectors[,1]),as.numeric(vectors[,2]))
  }
  for(i in 1:nrow(xy)){
  if(is.list(vectors)){
    i.pvalue = pvalue[i]
    i.labels = name[i]
    i.R2 = R2[i]
    i.col = mycolors[i.R2]
    i.lwd = NA
    if(i.pvalue<0.001){
      i.lwd = 3
	  i.lty = 1
    }else
      if(i.pvalue<0.01){
        i.lwd = 2
		i.lty = 2
      }else
        if(i.pvalue<0.05){
          i.lwd = 1
		  i.lty = 3
        }
	}else{
      i.labels = name[i]
      i.col = '#7F7F7F'
      i.lwd = 1		
	  i.lty = 1
	}	
    if(!is.na(i.lwd)){
      graphics::arrows(0,0,xy[i,1],xy[i,2],angle=30,code=2,lty=i.lty,lwd=i.lwd,col=i.col,length=0.105)
      text(xy[i,1],xy[i,2]+ayis.scale/3*sign(xy[i,2]),labels=i.labels,cex=0.8,font=3,col=i.col,adj=0.5)	  
    }
  }
  
  # 2 #
  par(mar = c(5,0,5,5))
  xn = ceiling(ng/4)
  plot(0, 0, type='l', xlim = c(-0.1,xn+0.35), ylim = c(0,10), xlab='', ylab='', axes = FALSE)  
  for(i in 1:xn){
    for(j in 1:4){
      if(j+4*(i-1)<=ng){
	    if(fill){
		  points(0.05+1*(i-1),j*0.5,col=col[j+4*(i-1)],pch=15,cex=2.5)
		}else{
		  points(0.05+1*(i-1),j*0.5,col=col[j+4*(i-1)],bg='grey100',pch=22,cex=2.5)		
		}    
        a = points.pch[j+4*(i-1)]
        # 2019.07.23
        #################
		if(!is.na(as.numeric(a))){
           a = as.numeric(a)
        }		  
        #################		
        points(0.05+1*(i-1),j*0.5,col=points.col[j+4*(i-1)],bg=points.bg[j+4*(i-1)],pch=a,cex=1)		  
        text(0.25+1*(i-1),j*0.5,labels=ngg[j+4*(i-1)],col='#7C7C7C',adj=0, cex=0.8, font=1)            
      }     
    }        
  }
  text(0,2.5,labels='Group',col='#7F7F7F',adj=0, cex=1.05, font=1)	
  if(is.list(vectors)){
  text(0,5.2,labels='P-value',col='#7F7F7F',adj=0, cex=1.05, font=1)
  graphics::arrows(0,4.5,0.3,4.5,angle=30,code=2,lty=1,lwd=3,col='#7F7F7F',length=0.08)
  graphics::arrows(0,4,0.3,4,angle=30,code=2,lty=2,lwd=2,col='#7F7F7F',length=0.08)
  graphics::arrows(0,3.5,0.3,3.5,angle=30,code=2,lty=3,lwd=1,col='#7F7F7F',length=0.08)
  text(0.4,4.5,labels='P < 0.001',col='#7C7C7C',adj=0, cex=0.65, font=3)
  text(0.4,4,labels='0.001 <= P < 0.01',col='#7C7C7C',adj=0, cex=0.65, font=3)
  text(0.4,3.5,labels='0.01 <= P < 0.05',col='#7C7C7C',adj=0, cex=0.65, font=3)	
  #mycolors = trans.colors(colorRampPalette(c('yellow','orange','red','red4'))(1000),TRUE,80)
  mycolors = trans.colors(colorRampPalette(c('#FFFDCB','#FFEB9B','#FCD775','#FDB04D','#FC8C3D','#FB4D2A','#E4161A','#BC0026','#800027'))(1000),TRUE,150)
  #text(0,7.2,labels='Explained variances (%)',col='black',adj=0, cex=1.2, font=1)	
  text(0,7.5,labels='Explained',col='#7F7F7F',adj=0, cex=1.05, font=1)
  text(0,7.15,labels='variances',col='#7F7F7F',adj=0, cex=1.05, font=1)
  left = cumsum(rep(1,times=1000))[1:999]/1000
  rigth = cumsum(rep(1,times=1000))[2:1000]/1000
  rect(left,rep(6.3,99),rigth,rep(6.6,99),col=mycolors,border=NA)
  text(1.05,6.45,labels='(%)',col='#7F7F7F',adj=0, cex=0.75, font=1)  
  segments(0,6.3,0,6.2,col='#7C7C7C')
  segments(0.5,6.3,0.5,6.2,col='#7C7C7C')
  segments(1,6.3,1,6.2,col='#7C7C7C')
  text(0,6,labels='0',cex=0.6,font=1,adj=0.5,col='#7C7C7C')
  text(0.5,6,labels='50',cex=0.6,font=1,adj=0.5,col='#7C7C7C')
  text(1,6,labels='100',cex=0.6,font=1,adj=0.5,col='#7C7C7C')
  }
}
