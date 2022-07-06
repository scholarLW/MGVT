conf.plot <- function(x, y, sd=TRUE, level=0.95, sigma=-1, alpha=NA, labels=NA, 
                      labels.font=1, labels.cex=1.5, labels.col='black', labels.adj=0.5,
                      col='darkgreen', pch=15, bg='darkgreen', cex=3,
                      lwd=5, lty=1, line.col="#7F7F7F", len=0.05,
                      plot=TRUE, ...){           # plot  
  if(!is.vector(x) || !is.vector(y)){
    return("x and y must be a vector!")
	stop()
  } 
  if(length(x) != length(y)){
    return("lengths of x and y must be same!")
	stop()
  }
  
  # ... further plot parameters.
  #require(Funset)
  if(sd){
	xinv = c(mean(x),0,mean(x)-sd(x),mean(x)+sd(x))
	yinv = c(mean(y),0,mean(y)-sd(y),mean(y)+sd(y))     
  }else{  
	xinv = as.numeric(confidencexy(x,level,sigma))
	yinv = as.numeric(confidencexy(y,level,sigma))
  }
  xlen = len
  ylen = len
  line.col = trans.colors(line.col,TRUE,100)
  labels.col = trans.colors(labels.col,TRUE,100)
  if(!is.na(alpha)){
    col = trans.colors(col,TRUE,alpha) 
    bg = trans.colors(bg,TRUE,alpha)
  }
  if(plot){
    plot(0, 0, type='l', xlim=c(min(x)-1,max(x)+1), ylim=c(min(y)-1,max(y)+1), ...)  
  }
  segments(xinv[3],yinv[1],xinv[4],yinv[1],lwd=lwd,lty=lty,col=line.col)
  segments(xinv[3],yinv[1]-ylen/2,xinv[3],yinv[1]+ylen/2,lwd=lwd,lty=lty,col=line.col)
  segments(xinv[4],yinv[1]-ylen/2,xinv[4],yinv[1]+ylen/2,lwd=lwd,lty=lty,col=line.col)
  segments(xinv[1],yinv[3],xinv[1],yinv[4],lwd=lwd,lty=lty,col=line.col)
  segments(xinv[1]-xlen/2,yinv[3],xinv[1]+xlen/2,yinv[3],lwd=lwd,lty=lty,col=line.col)
  segments(xinv[1]-xlen/2,yinv[4],xinv[1]+xlen/2,yinv[4],lwd=lwd,lty=lty,col=line.col)
  points(xinv[1],yinv[1],pch=pch,col=col,bg=bg,cex=cex)
  if(!is.na(labels)){
    text(xinv[1],yinv[4]+len*1.5,labels=labels,font=labels.font,cex=labels.cex,col=labels.col,adj=labels.adj)
  }
}

addP = function(centre,addEllipse){
  if(centre||addEllipse){
    FALSE
  }else{
    TRUE
  }
}

metacentre.plot = function(x, y=NULL, centre=FALSE, sd=TRUE, xlab='PC1', ylab='PC2', main='Principal coordinate analysis', xlim=NULL, ylim=NULL,
                           alpha=NA, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=1, 
                           add.labels=TRUE, labels.font=1, labels.cex=1, labels.col='black', labels.adj=0.5,
                           level=0.95, line.lwd=3, line.lty=1, line.col="#7F7F7F", len=NA, mycolors=NULL,
                           addEllipse=!centre, addPolygon=addP(centre,addEllipse), ...){
  
  # x "SampleID,Group,Dim1,Dim2"
  # y "Variables,Dim1,Dim2,R2,Pvalue" 
  # ... further parameters of Ellipse or Polygon. 
  
  require(Funset) 	
  if(centre){
    addEllipse=!centre
    addPolygon=!centre
  }
  if(addPolygon){
    addEllipse=!addPolygon
  }
  
  x = as.data.frame(x)
  rownames(x) = x[,c('SampleID')]
  group = as.character(x$Group)
  SampleID = as.character(x$SampleID)  
  st <- as.data.frame(x[,3:4])   # pc axis
  vectors = NULL
  if(!is.null(y)){
    vectors = as.data.frame(y) 
    rownames(vectors) = vectors[,c('Variables')] 
    vectors = vectors[,-1]
  }
  if(!is.null(vectors)){
    x = c(as.numeric(as.character(st[,1])),as.numeric(as.character(vectors[,1])))   
    y = c(as.numeric(as.character(st[,2])),as.numeric(as.character(vectors[,2])))   
  }else{
    x = as.numeric(as.character(st[,1])) 
    y = as.numeric(as.character(st[,2])) 	
  } 
  if(is.null(mycolors)){
      mycolors = c('#FFFDCB','#FFEB9B','#FCD775','#FDB04D','#FC8C3D','#FB4D2A','#E4161A','#BC0026','#800027')
  }  
  ngg = sort(unique(as.character(group)))
  ng = length(ngg)  
  if(is.null(points.col)){
    points.col = Colorpools()[1:ng]
  } 
  if(is.null(points.pch)){
    points.pch = Grouppointpools()[1:ng]  
  }   
  if(is.null(points.bg)){
    points.bg = points.col
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
  if(length(points.cex)){
    points.cex = rep(points.cex,ng)[1:ng]
  }    
  if(centre){
    x <- y <- NULL 
    for(i in 1:ng){    
      ix = group == ngg[i]
      x1 = as.numeric(as.character(st[ix,1]))
      y1 = as.numeric(as.character(st[ix,2]))
      if(!sd){
           sigma=-1
	       xinv = as.numeric(confidencexy(x1,level,sigma))
           yinv = as.numeric(confidencexy(y1,level,sigma))
		   x = c(x,xinv[3:4])
		   y = c(y,yinv[3:4])
      }else{  
		   x = c(x,mean(x1)-sd(x1),mean(x1)+sd(x1))
		   y = c(y,mean(y1)-sd(y1),mean(y1)+sd(y1))  
      }
    }   
  }

 axis.num = lim.set(x)
 ayis.num = lim.set(y)
 if(!is.null(xlim)){
    axis.num = lim.set(xlim)
 }  
 if(!is.null(ylim)){
   ayis.num = lim.set(ylim)
 }
 xmax = axis.num$r 
 xmin = axis.num$l
 ymin = ayis.num$l
 ymax = ayis.num$r
 axis.scale = axis.num$scale
 ayis.scale = ayis.num$scale
  
  if(is.na(len)){
    len = min(c(axis.scale,ayis.scale))/3
  }  
  if(!is.na(alpha)){
     points.col = trans.colors(points.col,TRUE,alpha)
	 points.bg = trans.colors(points.bg,TRUE,alpha)
  }  
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(11,5.5), heights=c(8,8))
  #layout.show(2)
  # 1 #
  par(mar = c(5,5,5,1))
  #plot(0, 0, type='l', axes=FALSE, xlim = c(xmin-axis.scale,xmax+1.5*axis.scale), ylim = c(ymin-ayis.scale,ymax+ayis.scale), xlab='',ylab='',main='')     
  plot(0, 0, type='l', axes=FALSE, xlim = c(xmin-axis.scale/1.2,xmax+axis.scale/1.2), ylim = c(ymin-ayis.scale/1.2,ymax+ayis.scale/1.2), xlab='',ylab='',main='')
  if(!centre){
    if(is.na(alpha)){
      alpha = 150
    }	
    if(addEllipse){     
      MGVT::ellipse(st,group=group,alpha=alpha,...) 
    }	
    if(addPolygon){
      points = cbind(rownames(st),group,st)
      colnames(points) = c('SampleID','Group','x','y')
      minPolygon.plot(points, alpha=alpha, plot=FALSE,addPoints=FALSE,...)
    }	
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
  }else{
    for(i in 1:ng){    
      ix = group == ngg[i]
      x = as.numeric(as.character(st[ix,1]))
      y = as.numeric(as.character(st[ix,2]))
      if(!add.labels){
        labels = NA
      }else{
        labels = ngg[i]
      }
	  a = points.pch[i]
	  # 2019.07.23
	  #################
		if(!is.na(as.numeric(a))){
           a = as.numeric(a)
        }		  
	  #################		  
      conf.plot(x, y, sd=sd, level=level, sigma=sigma, alpha=alpha, labels=labels, 
                labels.font=labels.font, labels.cex=labels.cex, labels.col=labels.col, labels.adj=labels.adj,
                col=points.col[i], pch=a, bg=points.bg[i], cex=points.cex[i],
                lwd=line.lwd, lty=line.lty, line.col=line.col, len=len,
                plot=FALSE)	  
    }           	
  }  
  
  segments(0,ymin,0,ymax,lty=2,lwd=0.8)
  segments(xmin,0,xmax,0,lty=2,lwd=0.8)	
  x = seq(xmin,xmax,axis.scale)
  y = seq(ymin,ymax,ayis.scale)
  segments(xmin-axis.scale/9,y[-length(y)][-1],xmin,y[-length(y)][-1],lty=1,lwd=1,col=trans.colors('grey40',TRUE,200))
  segments(x[-length(x)][-1],ymin-ayis.scale/6.5,x[-length(x)][-1],ymin,lty=1,lwd=1,col=trans.colors('grey40',TRUE,200))
  segments(xmin,y[-length(y)],xmax,y[-length(y)],lty=1,lwd=2,col=trans.colors('grey90',TRUE,60))
  segments(x[-length(x)],ymin,x[-length(x)],ymax,lty=1,lwd=2,col=trans.colors('grey90',TRUE,60))		
  text(x[-length(x)][-1],ymin-ayis.scale/2,labels=x[-length(x)][-1],col='grey40',cex=0.8)
  text(xmin-axis.scale/1.7,y[-length(y)][-1],labels=y[-length(y)][-1],col='grey40',cex=0.8)	
  mtext(ylab,side=2,col='grey40',cex=1,las=3)	
  mtext(xlab,side=1,col='grey40',cex=1)	
  mtext(main,side=3,col='grey',cex=1.5,adj=0.5,padj=0)
  
  if(!is.null(vectors)){
    name = as.character(rownames(vectors))
    xy = cbind(as.numeric(as.character(vectors[,1])),as.numeric(as.character(vectors[,2])))
    if('R2' %in% colnames(vectors)){
      R2 = as.numeric(as.character(vectors[,3]))
      R2 = round(R2,2)*100
      mycolors = trans.colors(colorRampPalette(mycolors)(100),TRUE,150)	
    }
    if('Pvalue' %in% colnames(vectors)){
      pvalue = as.numeric(as.character(vectors[,4]))
    }
    
    for(i in 1:nrow(xy)){
      if(!is.null(vectors)){
        i.labels = name[i]
        if('R2' %in% colnames(vectors)){
          i.R2 = R2[i]
          i.col = mycolors[i.R2]	
        }else{
          i.col = '#7F7F7F'	
        }
        if('Pvalue' %in% colnames(vectors)){
          i.pvalue = pvalue[i]
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
          i.lwd = 1
          i.lty = 1
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
  }
  
  # 2 #
  par(mar = c(5,0,5,5))
  xn = ceiling(ng/4)
  plot(0, 0, type='l', xlim = c(-0.1,xn+0.35), ylim = c(0,10), xlab='', ylab='', axes = FALSE)  
  for(i in 1:xn){
    for(j in 1:4){
      if(j+4*(i-1)<=ng){
        points(0.05+1*(i-1),j*0.5,col=points.col[j+4*(i-1)],bg='grey100',pch=22,cex=2.5)
        a = points.pch[j+4*(i-1)]
        # 2019.07.23
        #################
		if(!is.na(as.numeric(a))){
           a = as.numeric(a)
        }		  
        #################			
        points(0.05+1*(i-1),j*0.5,col=points.col[j+4*(i-1)],bg=points.bg[j+4*(i-1)],pch=a,cex=1.2)		  
        text(0.25+1*(i-1),j*0.5,labels=ngg[j+4*(i-1)],col='#7C7C7C',adj=0, cex=0.8, font=1)            
      }     
    }        
  }
  text(0,2.5,labels='Group',col='#7F7F7F',adj=0, cex=1.05, font=1)	
  
  if(!is.null(vectors)){
    if('R2' %in% colnames(vectors)){
      text(0,5.2,labels='P-value',col='#7F7F7F',adj=0, cex=1.05, font=1)
      graphics::arrows(0,4.5,0.3,4.5,angle=30,code=2,lty=1,lwd=3,col='#7F7F7F',length=0.08)
      graphics::arrows(0,4,0.3,4,angle=30,code=2,lty=2,lwd=2,col='#7F7F7F',length=0.08)
      graphics::arrows(0,3.5,0.3,3.5,angle=30,code=2,lty=3,lwd=1,col='#7F7F7F',length=0.08)
      text(0.4,4.5,labels='P < 0.001',col='#7C7C7C',adj=0, cex=0.65, font=3)
      text(0.4,4,labels='0.001 <= P < 0.01',col='#7C7C7C',adj=0, cex=0.65, font=3)
      text(0.4,3.5,labels='0.01 <= P < 0.05',col='#7C7C7C',adj=0, cex=0.65, font=3)	
      mycolors = trans.colors(colorRampPalette(mycolors)(1000),TRUE,150)
    }
    if('Pvalue' %in% colnames(vectors)){
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
}
