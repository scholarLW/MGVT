dataPC3D.plot <- function(x=NULL, y=NULL, f.selectcolumn=1, D3=FALSE, D3.method=NULL, D3.col=NULL, D3.alpha=150, D3.pch=NULL, 
                          D2.selectcol=NULL, D2.axis.scale=NULL, D2.ayis.scale=NULL, D2.axes=TRUE, D2.sub.title=NULL,
                          cex=1, ...){
  
  # ... further plot/scatter3D/scatterplot3d parameters.
  
  nameID = colnames(x)  
  if(! 'SampleID' %in% nameID){
    return("Please enter D3.column 'SampleID' for matrix x!")
	stop()
  }
  if(! 'Group' %in% nameID){
    return("Please enter D3.column 'Group' for matrix x!")
	stop()
  } 
  if(! 'SampleID' %in% colnames(y)){
    return("Please enter D3.column 'SampleID' for matrix y!")
	stop()
  }
  ngg = sort(unique(as.character(x$Group)))
  ng = length(ngg)  
  if(is.null(D3.col)){
     D3.col = Colorpools()[1:ng]  
  }
  if(is.null(D3.pch)){
    D3.pch = Grouppointpools()[1:ng]
  }  
  if(length(D3.col)<ng){
    D3.col = rep(D3.col,ng)[1:ng]
  }  
  if(length(D3.pch)<ng){
    D3.pch = rep(D3.pch,ng)[1:ng]
  }   
  n = length(D3.col)
  for(i in 1:n){
    gD3.color = D3.col[i]
    if(!grepl(',',gD3.color)){
      data(colorpools)
      if(grepl('#',gD3.color)){
        ix = color$n16 == gD3.color
        if(sum(ix)>0){
          D3.col[i] = color$nrgb[ix][1]
        }        
      }else
        if(! grepl(',',gD3.color)){
          ix = color$nrgb == gD3.color
          if(sum(ix)>0){
            D3.col[i] = color$nrgb[ix][1]
          }                    
        }
    }
  }
  D3.col = trans.colors(D3.col,TRUE,D3.alpha)
  if(is.null(D2.selectcol)){
    D2.selectcol = 0
  }
  if(is.null(D2.axis.scale)){
    D2.axis.scale = 0.2      
  }
  if(is.null(D2.ayis.scale)){
    D2.ayis.scale = 0.2      
  }  
  if(is.null(D3.method)){
    D3.method = 'plot3D'
  }
  y = y[,c(1,(1+f.selectcolumn))]
  ix = is.na(y[,2])
  y = y[!ix,]
  data = merge(x[,1:4],y,by=c('SampleID'),all=F,sort=F)
  featureName = colnames(y)[2]  
  if(is.null(D2.sub.title)){
    D2.sub.title = paste("Abundance of ",featureName,sep="")
  }
  data = as.data.frame(data)
  # scale
  data$x = as.numeric(data$x)
  data$y = as.numeric(data$y)
  data[,5] = as.numeric(data[,5])  # data ---> SampleID Group x y featureName
  if(sum(data$x<0)>0){
    xmax_l = max(abs(data$x[data$x<0]))
  }else{
    xmax_l = 0
  }
  if(sum(data$x>0)>0){
    xmax_r = max(abs(data$x[data$x>0]))
  }else{
    xmax_r = 0
  }
  if(sum(data$y<0)>0){
    ymax_l = max(abs(data$y[data$y<0]))
  }else{
    ymax_l = 0
  }
  if(sum(data$y>0)>0){
    ymax_r = max(abs(data$y[data$y>0]))
  }else{
    ymax_r = 0
  }
  xnl = ceiling(xmax_l/D2.axis.scale)
  xnr = ceiling(xmax_r/D2.axis.scale)
  ynl = ceiling(ymax_l/D2.ayis.scale)
  ynr = ceiling(ymax_r/D2.ayis.scale)
 
  # data ---> SampleID Group x y featureName
  if(D3){
    data$pch = NA
    data$color = NA
    for(i in 1:ng){
      ix = data$Group == ngg[i]
      data$pch[ix] = D3.pch[i]
      data$color[ix] = D3.col[i]
    }
    data = data[order(data[,5]),]
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(8,4), heights=c(8,8))
    #layout.show(2)
    # 1 #
    par(mar = c(5,5,5,0))
    if(D3.method == 'scatterplot3d'){
      require(scatterplot3d)
      scatterplot3d(data$x,data$y,data[,5],color=data$color,pch=data$pch,bg=data$color,cex.symbols=cex, ...)
    }else{
      require(plot3D)
      scatter3D(data$x,data$y,data[,5],col=data$color,pch=data$pch,bg=data$color,colvar=FALSE,cex=cex, ...)
    }
    # 2 #
    par(mar = c(5,0,5,5))
    plot(0, 0, type='l', xlim = c(0,1), ylim = c(-ng,ng+1), xlab='', ylab='', axes = FALSE)   
    for(i in 1:ng){
      y1 = i*0.5
      colg = D3.col[i]
      points(0.1, y1+0.05, pch=D3.pch[i], bg=colg, col=colg, cex=1.2)
      text(0.45, y1+0.05,labels=ngg[i],col='black', cex=1, font=2)       
    }
  }else{
    data[,5] = ceiling(data[,5])
    max_d = max(data[,5])
    scale = 100/max_d
    data[,5] = data[,5] * scale
    data[,5][data[,5]>100] = 100  
    data[,5][data[,5]==0] = 1  
    if(D2.selectcol==1){
      col <- rev(colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F","cyan","#007FFF","blue","#00007F"))(100))
    }else
      if(D2.selectcol==0){
      col <- colorRampPalette(c('#292C7C','#ACD9EB','#54B947','#B4D234','#F2EB19','#F9A41B','#8A1619'))(100)
      }else{
        return("D2.selectcol should be 0 or 1!")
		stop()
      }

    layout(matrix(data=c(2,1), nrow=2, ncol=1), widths=c(8,8), heights=c(2,8))
    #layout.show(2)
    # 1 #
    par(mar = c(5,5,1,5))
    if(D2.axes){
      plot(0, 0, type='l', axes=TRUE, xlim = c(-xnl*D2.axis.scale,xnr*D2.axis.scale), ylim = c(-ynl*D2.ayis.scale,ynr*D2.ayis.scale), ...)     
    }else{
      plot(0, 0, type='l', axes=FALSE, xlim = c(-xnl*D2.axis.scale,xnr*D2.axis.scale), ylim = c(-ynl*D2.ayis.scale,ynr*D2.ayis.scale), ...)
      axis(1,at=seq(-xnl*D2.axis.scale,xnr*D2.axis.scale,D2.axis.scale),labels=seq(-xnl*D2.axis.scale,xnr*D2.axis.scale,D2.axis.scale),col="black",col.axis="black",lwd=2)
      axis(2,at=seq(-ynl*D2.ayis.scale,ynr*D2.ayis.scale,D2.ayis.scale),labels=seq(-ynl*D2.ayis.scale,ynr*D2.ayis.scale,D2.ayis.scale),col="black",col.axis="black",lwd=2)
    }
    points(data$x,data$y,pch=20,col=col[data[,5]],bg=col[data[,5]],cex=cex)  
    # 2 #
    par(mar = c(0,5,5,5))
    if(D2.selectcol==1){
      mycolors = rev(colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F","cyan","#007FFF","blue","#00007F"))(100))
    }else
      if(D2.selectcol==0){
        mycolors = colorRampPalette(c('#292C7C','#ACD9EB','#54B947','#B4D234','#F2EB19','#F9A41B','#8A1619'))(100)
      }else{
        return("D2.selectcol should be 0 or 1!")
		stop()
      }
    plot(0, 0, type='l', xlim = c(-0.2,1.2), ylim = c(-1,1), xlab='', ylab='', axes = FALSE)
    left = cumsum(rep(1,times=100))[1:99]/100
    rigth = cumsum(rep(1,times=100))[2:100]/100
    rect(left,rep(0.1,99),rigth,rep(0.9,99),col=mycolors,border=NA)
    segments(left[1],0.1,left[1],0.9,lwd=1)
    segments(left[1],0.1,rigth[99],0.1,lwd=1)
    segments(rigth[99],0.1,rigth[99],0.9,lwd=1)
    segments(left[1],0.9,rigth[99],0.9,lwd=1)
    mtext(side=3,D2.sub.title,line=0,col="black",font=2,cex=1.1)   
    xs = seq(0.2,0.8,0.2)
    segments(xs,rep(-0.1,length(xs)),xs,rep(0.1,length(xs)),lwd=2)
    lables = xs*max_d
    text(xs,-0.5,labels=lables,cex=0.8)
  }
}   
