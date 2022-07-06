violin.plot = function(x, group=NULL, coordinate.lab='Value', coordinate.lim=NULL, main='Violin plot', alpha=NA, col=NULL, rectCol=NULL, ...){
  
  # ... further parameters of vioplot.  lty, lwd, lineCol, pchMed, colMed, colMed2, outer
  require(vioplot)  
  
  # x   N x 2(3), columns: "SampleID,x" or "SampleID,Group,x"
  x = as.data.frame(x)
  ng = 1
  ngg = 'g1'     
  if(!(is.matrix(x) || is.array(x) || is.data.frame(x))){
    return("Error inputdata!")
    stop()	
  }  
  if(!is.null(group)){    
    if(length(group) != nrow(x)){
      return("rows of x and group must be the same length")
      stop()	  
    }
  }else{
    if(!'Group' %in% colnames(x)){
      return("if group is null, then x should have the columns 'Group'")
	  stop()
    }else{
	  group = as.character(x$Group)
	}  
  }
  ngg = sort(as.character(unique(group)))
  ng = length(ngg) # group is a vector with the same length of x
  y = as.numeric(as.character(x$x))
  
  if(is.null(col)){    
    col = gpColorpools()[1:ng]
  }
  if(is.null(rectCol)){
    rectCol = Colorpools()[1:ng]
  }
  if(length(col)){
    col = rep(col,ng)[1:ng] 
  }
  if(length(rectCol)){
    rectCol = rep(rectCol,ng)[1:ng] 
  }
  if(!is.na(alpha)){
    col = trans.colors(col,TRUE,alpha) 
  }else{
    col = trans.colors(col,FALSE,0) 
  }  
  rectCol = trans.colors(rectCol,FALSE,240) 
  if(!is.null(coordinate.lim)){
    ymax = max(coordinate.lim)
	ymix = min(coordinate.lim)
  }else{
    ymax = round(max(y),1)+0.05
    ymin = round(min(y),1)-0.05 
  }  
  ayis.scale = (ymax-ymin)/5  
  if(ng > 1){
    layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(10,4), heights=c(8,8))
    #layout.show(2)
    # 1 #
    par(mar = c(5,5,5,0)) 
  }else{
    par(mar = c(5,5,5,5)) 
  }	
  plot(0, 0, type='l', axes=FALSE, xlim = c(-1,ng+0.5), ylim = c(ymin-ayis.scale/2,ymax+ayis.scale), xlab='',ylab='',main='')     

  for(i in 1:ng){    
      ix = group == ngg[i]
      i.y = as.numeric(as.character(y[ix]))
      vioplot(i.y, at=i-1, add = TRUE, border=NA, axes=FALSE, drawRect=TRUE, col = col[i], rectCol = rectCol[i], pchMed=15, ...)
  }    

  # x 
  #axis(1,at=seq(0,ng-1,1),labels=ngg,col=trans.colors('grey40',TRUE,200),col.axis=trans.colors('grey40',TRUE,200),lwd=2,las=3)  
  
  x = seq(0,ng-1,1)
  y = seq(ymin,ymax,ayis.scale) 
  segments(-0.65,y[-length(y)][-1],-0.6,y[-length(y)][-1],lty=1,lwd=1,col=trans.colors('grey40',TRUE,200))
  segments(x,ymin-ayis.scale/8,x,ymin,lty=1,lwd=1,col=trans.colors('grey40',TRUE,200))
  segments(-0.6,y[-length(y)],ng,y[-length(y)],lty=1,lwd=2,col=trans.colors('grey90',TRUE,60)) 
  text(x,ymin-ayis.scale/4,labels=ngg,col='grey40',cex=0.8)
  x = seq(0,ng,0.5)
  segments(x[-length(x)],ymin,x[-length(x)],ymax,lty=1,lwd=2,col=trans.colors('grey90',TRUE,60))		
  segments(-0.6,ymin,-0.6,ymax,lty=1,lwd=2,col=trans.colors('grey90',TRUE,60)) 
  text(-0.9,y[-length(y)][-1],labels=y[-length(y)][-1],col='grey40',cex=0.8)	
  mtext(coordinate.lab,side=2,col='grey40',cex=1,las=3)		
  mtext(main,side=3,col='grey',cex=1.5,adj=0.5,padj=0)

  # 2 #
  if(ng>1){
  par(mar = c(5,0,5,5))
  xn = ceiling(ng/4)
  plot(0, 0, type='l', xlim = c(-0.1,xn+0.35), ylim = c(0,10), xlab='', ylab='', axes = FALSE)  
  for(i in 1:xn){
    for(j in 1:4){
      if(j+4*(i-1)<=ng){
        points(0.05+1*(i-1),j*0.5,col=col[j+4*(i-1)],bg=col[j+4*(i-1)],pch=22,cex=2.5)		      
        points(0.05+1*(i-1),j*0.5,col=rectCol[j+4*(i-1)],bg=rectCol[j+4*(i-1)],pch=15,cex=1.2)		  
        text(0.3+1*(i-1),j*0.5,labels=ngg[j+4*(i-1)],col='#7C7C7C',adj=0, cex=0.8, font=1)            
      }     
    }        
  }
  text(0,2.5,labels='Group',col='#7F7F7F',adj=0, cex=1.05, font=1)	      
  }
}
