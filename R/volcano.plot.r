volcano.plot = function(x, y=NULL, xlab='log2(FoldChange)', ylab='-log10(P-value)', main='Volcano plot',
                           alpha=NA, pch=NULL, col=c('#FFCC66','#000000','#FF6666'), bg=col, cex=1, log2FCth=1, pvalueth=0.05,
						   line.col='grey', line.lwd=2, line.lty=2){
  
  # x  N x 2 [log2FC,pvalue] or a vectors
  # y  a vectors
  
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
   ix = is.na(x) | is.na(y) | is.infinite(x) | is.infinite(y) | is.nan(x) | is.nan(y)
   x = as.numeric(as.character(x[!ix]))
   y = as.numeric(as.character(y[!ix]))
   y1 = y
   if(length(x)<1){
     return("Unqualified inputdata!")
	 stop()
   }
	if(any(y<=1)){
     y = -log10(y)
  }      
  xmax = ceiling(max(abs(x)))
  ymax = ceiling(max(abs(y)))
  axis.scale =  xmax/5
  ayis.scale = ymax/5
  if(!is.na(alpha)){
     col = trans.colors(col,TRUE,alpha)
	 bg = trans.colors(bg,TRUE,alpha)
  }
  if(is.null(pch)){
     pch = 20
  }
 if(length(col)){
    col = rep(col,3)[1:3] 
 }
 if(length(pch)){
    pch = rep(pch,3)[1:3] 
 }
 if(length(bg)){
    bg = rep(bg,3)[1:3] 
 } 
 if(length(cex)){
    cex = rep(cex,3)[1:3] 
 } 
  par(mar = c(5,5,5,5))
  plot(0, 0, type='l', axes=FALSE, xlim = c(-xmax-axis.scale,xmax+axis.scale), ylim = c(0-ayis.scale/2,ymax+ayis.scale), xlab='',ylab='',main='')     
  # points
  #group.mark <- p.bg <- p.col <- p.cex <- NULL  # 2019.07.23
  for(i in 1:length(x)){ 
    group.mark <- p.bg <- p.col <- p.cex <- NULL  # 2019.07.23
      if(x[i] < -log2FCth & y1[i] < pvalueth){
	        group.mark = c(group.mark,pch[1])
	        p.bg = c(p.bg,bg[1])
	        p.col = c(p.col,col[1])
	        p.cex = c(p.cex,cex[1])
	  }else
	  if(x[i] > log2FCth & y1[i] < pvalueth){
	        group.mark = c(group.mark,pch[3])
	        p.bg = c(p.bg,bg[3])
	        p.col = c(p.col,col[3])
	        p.cex = c(p.cex,cex[3])	  
	  }else{
	        group.mark = c(group.mark,pch[2])
	        p.bg = c(p.bg,bg[2])
	        p.col = c(p.col,col[2])
	        p.cex = c(p.cex,cex[2])
	  }
	 # 2019.07.23
     ####################	 
	 if(!is.na(as.numeric(group.mark))){
       group.mark = as.numeric(group.mark)
     }	  
	  points(x[i],y[i],pch=group.mark,bg=p.bg,col=p.col,cex=p.cex)
	 #################### 
  }
  #points(x,y,pch=group.mark,bg=p.bg,col=p.col,cex=p.cex)    # 2019.07.23	 
  segments(-xmax, -log10(pvalueth), xmax, -log10(pvalueth), col=line.col, lwd=line.lwd, lty=line.lty)
  segments(-log2FCth, 0, -log2FCth, ymax, col=line.col, lwd=line.lwd, lty=line.lty)
  segments(log2FCth, 0, log2FCth, ymax, col=line.col, lwd=line.lwd, lty=line.lty)
  segments(-xmax,0,xmax,0,lty=1,lwd=2.5,col=trans.colors('grey90',TRUE,60))	
  x = seq(-xmax,xmax,axis.scale)
  y = seq(0,ymax,ayis.scale)
  segments(-xmax-axis.scale/9,y[-length(y)][-1],-xmax,y[-length(y)][-1],lty=1,lwd=1,col=trans.colors('grey40',TRUE,200))
  segments(x[-length(x)][-1],0-ayis.scale/10,x[-length(x)][-1],0,lty=1,lwd=1,col=trans.colors('grey40',TRUE,200))
  segments(-xmax,y[-length(y)],xmax,y[-length(y)],lty=1,lwd=2.5,col=trans.colors('grey90',TRUE,60))
  segments(x[-length(x)],0,x[-length(x)],ymax,lty=1,lwd=2.5,col=trans.colors('grey90',TRUE,60))		
  text(x[-length(x)][-1],-0-ayis.scale/3.5,labels=x[-length(x)][-1],col='grey40',cex=0.8)
  text(-xmax-axis.scale/1.7,y[-length(y)][-1],labels=y[-length(y)][-1],col='grey40',cex=0.8)	
  mtext(ylab,side=2,col='grey40',cex=1,las=3)	
  mtext(xlab,side=1,col='grey40',cex=1)	
  mtext(main,side=3,col='grey',cex=1.5,adj=0.5,padj=0)
}
