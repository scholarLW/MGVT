box.p = function(x,counts,l,col,fill,border,lwd,whisker.lty=1){
  res = outliers(counts)
  len = l/3
  dot.5 = res$dot.5
  col1 = col
  xline = c(x-len,x+len,x+len,x-len,x-len)
  yline = c(dot.5[2],dot.5[2],dot.5[4],dot.5[4],dot.5[2])
  if(fill){
    col1='black'
    polygon(cbind(xline,yline), col=col, border=border)
  }
  lines(cbind(xline,yline), col=col1, lwd=lwd)
  if(fill){
    segments(x-len,dot.5[3],x+len,dot.5[3],col=col1,lwd=2*lwd)
  }else{
    segments(x-len,dot.5[3],x+len,dot.5[3],col=col1,lwd=lwd)
  }  
  segments(x,dot.5[1],x,dot.5[2],col=col1,lwd=lwd,lty=whisker.lty)
  segments(x,dot.5[4],x,dot.5[5],col=col1,lwd=lwd,lty=whisker.lty)
  if(whisker.lty!=1){
    segments(x-len/3,dot.5[1],x+len/3,dot.5[1],col=col1,lwd=lwd)
    segments(x-len/3,dot.5[5],x+len/3,dot.5[5],col=col1,lwd=lwd)
  }
}

permutation.comb = function(n){
  if(n<2){
    return("Not enough points!")
	stop()
  }
  num = factorial(n)/factorial(2)
  da = NULL
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      da = as.data.frame(rbind(da,cbind(i,j)))
    }
  }
  return(da)
}

boxline <- function(data, col=NULL, fill=FALSE, border=NA, lwd=2, whisker.lty=2, alpha=150, lim=NULL, lab=NULL, main=NULL, 
                    line.add=FALSE, line.col=NULL, line.lty=NULL, line.lwd=0.5, axis.lwd=2,
                    points.add=TRUE, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=0.7, points.random=!line.add,
                    text.adj=0, text.cex=1, text.col=NULL, text.font=NULL,                     
                    t.interGroup=!line.add, t.intraGroup=!t.interGroup, t.method=c('Student','Wilcoxon'), pval=FALSE, ...){
  
  # ... further parameters of statistical testing method. 
  require(Funset)
  
  
  #Input 
  #HostID Group sample1-value  sample2-value ... sampleN-value   #HostID must require,sample1-value  sample2-value ... are the time series sample of host
  
  data = as.data.frame(data)
  nameID = colnames(data)
  ng = 1
  ngg = NULL   
  if(!"Group" %in% nameID){
    data$Group = 'OnlyOneGroup'
    t.interGroup = FALSE
  }
  data = data[order(data$Group),]
  
  if(!"HostID" %in% nameID){
    return("HostID column must require!")
	stop()
  }
  if(t.interGroup){
    t.intraGroup = FALSE
  }
  if(line.add){
    points.random = FALSE
  }  
  if(length(t.method)>1){
    t.method = 'Student'
  }
  
  nameID = colnames(data)
  data$Group = as.character(data$Group)
  ngg = sort(unique(as.character(data$Group)))
  ng = length(ngg) 
  if(ng>10){
    t.interGroup = FALSE
    t.intraGroup = FALSE
  }  
  if(is.null(col)){
    if(fill){
      col = gpColorpools()[1:ng]
    }else{
      col = Colorpools()[1:ng]  
    }
  } 
  if(is.null(points.col)){
    points.col = 'grey'
  } 
  if(is.null(line.col)){
    line.col = points.col
  }  
  if(is.null(text.col)){
    text.col = 'black'
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
  if(length(text.col)){
    text.col = rep(text.col,ng)[1:ng]
  }  
  if(length(line.col)){
    line.col = rep(line.col,2)[1:2]   # 1) slope >=0 or infinity and 2) slope < 0
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
  line.col = trans.colors(line.col,FALSE,0)  
  
  # scale  
  n = ncol(data)-2
  tmp = data[,-which(colnames(data)=='Group')]
  tmp = tmp[,-which(colnames(tmp)=='HostID')] 
  tmp = as.matrix(tmp)
  data = as.data.frame(cbind(as.character(data$HostID),as.character(data$Group),tmp))
  colnames(data)[1:2] = c('HostID','Group')
  
  tmp.y <- NULL
  if(n == 1){
    tmp.y = as.numeric(as.character(tmp))
  }else{
    for(i in 1:n){
      tmp.y = c(tmp.y,as.numeric(as.character(tmp[,i])))
    }
  }
  if(is.null(lim)){
    dis = max(tmp.y) - min(tmp.y)
  }else{
    dis = lim[2] - lim[1]
    ix = tmp.y >= lim[1] & tmp.y <= lim[2]
    tmp.y = tmp.y[ix]
  }
  axis.scale = round(dis/(ng*n+1),2)
  ayis.scale = round(dis/4,2)
  ynl = ceiling(min(tmp.y)/ayis.scale)-1
  ynr = ceiling(max(tmp.y)/ayis.scale)
  
  xlim = c(0,ng*n + 1)*axis.scale
  ylim = c(ynl*ayis.scale,(ynr+0.2)*ayis.scale)
  layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(8,4), heights=c(8))
  #layout.show(2)
  # 1 #
  par(mar = c(5,5,5,0))
  if(is.null(lab)){
    plot(0, 0, type='l', axes=FALSE, xlim = xlim, ylim = ylim, xlab='', ylab='', main=main) 
  }else{
    plot(0, 0, type='l', axes=FALSE, xlim = xlim, ylim = ylim, xlab='', ylab=lab, main=main) 
  }
  segments(xlim[1],ylim[1],xlim[2],ylim[1],lwd=lwd/2,col='black')
  segments(xlim[1],ylim[1],xlim[1],ylim[2],lwd=lwd/2,col='black')
  segments(xlim[2],ylim[1],xlim[2],ylim[2],lwd=lwd/2,col='black')
  segments(xlim[1],ylim[2],xlim[2],ylim[2],lwd=lwd/2,col='black')  
  
  #axis(2,at=seq(ynl*ayis.scale,ynr*ayis.scale,ayis.scale),labels=seq(ynl*ayis.scale,ynr*ayis.scale,ayis.scale),col="black",col.axis="black",lwd=1.2)
  tmp.k = ceiling(log10(ayis.scale)-1)
  tmp.ayis.scale = 10^ceiling(log10(ayis.scale)-1)*round(10^(-tmp.k)*ayis.scale)
  if(ynl*tmp.ayis.scale<ylim[1]){
    ynl=ynl+1
  }
  if(ynr*tmp.ayis.scale>ylim[2]){
    ynr=ynr-1
  }  
  axis(2,at=seq(ynl*tmp.ayis.scale,ynr*tmp.ayis.scale,tmp.ayis.scale),labels=seq(ynl*tmp.ayis.scale,ynr*tmp.ayis.scale,tmp.ayis.scale),col="black",col.axis="black",lwd=axis.lwd)
  # x 
  labels <- NULL
  nameID = nameID[-which(nameID=='Group')]
  nameID = nameID[-which(nameID=='HostID')]
  if(ng == 1){
    for(j in 1:n){
      labels = nameID 
    }
  }else{
    if(n == 1){
      labels = ngg 
    }else{
      for(i in 1:ng){
        for(j in 1:n){
          labels = c(labels,paste(ngg[i],'_',nameID[j],sep='')) 
        }
      }	
    }
  }
  axis(1,at=seq(axis.scale,ng*n*axis.scale,axis.scale),labels=labels,col="black",col.axis="black",lwd=axis.lwd,las=3)  # las = 2 or 3
  
  #line plot
  if(line.add && n>1){
    for(i in 1:ng){
      ix = data$Group == ngg[i]
      tmp = data[ix,-c(1:2)]
      pcol = line.col[i]
      left = 1:(n-1)
      rigth = 2:n
      for(j in 1:(n-1)){
        counts_j = as.numeric(as.character(tmp[,left[j]]))
        counts_j1 = as.numeric(as.character(tmp[,rigth[j]]))
        segments(rep((left[j]+(i-1)*n)*axis.scale,length(counts_j)),counts_j,rep((rigth[j]+(i-1)*n)*axis.scale,length(counts_j1)),counts_j1,col=pcol,lty=line.lty,lwd=line.lwd)
      }
    }
  }
  
  #box plot
  for(i in 1:ng){
    ix = data$Group == ngg[i]
    tmp = data[ix,-c(1:2)]
    pcol = points.col[i]
    pbg = points.bg[i]
    ppch = points.pch[i]
	# 2019.07.23
	#################
	if(!is.na(as.numeric(ppch))){
       ppch = as.numeric(ppch)
    }	
	##################	
    for(j in 1:n){
      if(n == 1){
        counts = as.numeric(as.character(tmp))
      }else{
        counts = as.numeric(as.character(tmp[,j]))
      }
      box.p((j+(i-1)*n)*axis.scale,counts,axis.scale,col[i],fill,border,lwd,whisker.lty)
      #points plot
      if(points.add){
        x = rep((j+(i-1)*n)*axis.scale,length(counts))
        if(points.random){
          x = x + rnorm(length(counts),mean=0,sd=1)*axis.scale/20
        }
        points(x,counts,pch=ppch, bg=pbg, col=pcol, cex=points.cex)
      }
    }
  }
  
  # p test plot
  
  if(t.interGroup && n==1){
    index = permutation.comb(ng)
    n.index = nrow(index)   
    for(i in 1:n.index){
      ix = data$Group == ngg[index[i,1]]
      ij = data$Group == ngg[index[i,2]]
      tmp_ix = as.numeric(as.character(data[ix,-c(1:2)]))
      tmp_ij = as.numeric(as.character(data[ij,-c(1:2)]))
      len = ayis.scale/20
      ymax = max(c(tmp_ix,tmp_ij))+(i-1)*ayis.scale/5
      if(t.method %in% 'Student'){
        p.value = t.test(tmp_ix, tmp_ij, ...)$p.value
      }
      if(t.method %in% 'Wilcoxon'){
        p.value = wilcox.test(tmp_ix, tmp_ij, ...)$p.value
      }
      if(p.value<0.05){
        # segments
        x1=(1+(index[i,1]-1)*n)*axis.scale
        x2=(1+(index[i,2]-1)*n)*axis.scale
        y1=ymax-len/2
        y12=ymax
        y2=ymax+len/2
        segments(x1,y1,x1,y2,col='black',lwd=lwd/2)
        segments(x2,y1,x2,y2,col='black',lwd=lwd/2)
        segments(x1,y12,x2,y12,col='black',lwd=lwd/2)
        # text
        if(pval){
          enum = ceiling(log10(p.value)-1)
          fnum = round(p.value*10^abs(enum),2)
          text((x2+x1)/2,y2+len,labels=paste("P=",fnum,'e',enum,sep=""),font=3,cex=0.8)
        }else{
          if(pval<0.001){
            text((x2+x1)/2,y2+len,labels="***",font=3,cex=0.8)
          }else
            if(pval<0.01){
              text((x2+x1)/2,y2+len,labels="**",font=3,cex=0.8)
            }else{
              text((x2+x1)/2,y2+len,labels="*",font=3,cex=0.8)
            }
        }
      }
    } 
  }
  
  if(t.intraGroup && n>1){
    for(i in 1:ng){
      ix = data$Group == ngg[i]
      tmp = data[ix,-c(1:2)]
      left = 1:(n-1)
      rigth = 2:n
      for(j in 1:(n-1)){
        tmp_ix = as.numeric(as.character(tmp[,left[j]]))
        tmp_ij = as.numeric(as.character(tmp[,rigth[j]]))
        len = ayis.scale/20
        ymax = max(tmp.y)
        if(t.method %in% 'Student'){
          p.value = t.test(tmp_ix,tmp_ij,...)$p.value
        }
        if(t.method %in% 'Wilcoxon'){
          p.value = wilcox.test(tmp_ix,tmp_ij,...)$p.value
        }
        
        if(p.value<0.05){
          # segments
          x1=(left[j]+(i-1)*n)*axis.scale
          x2=(rigth[j]+(i-1)*n)*axis.scale
          y1=ymax-len/2
          y12=ymax
          y2=ymax+len/2
          segments(x1,y1,x1,y2,col='black',lwd=lwd/2)
          segments(x2,y1,x2,y2,col='black',lwd=lwd/2)
          segments(x1,y12,x2,y12,col='black',lwd=lwd/2)
          # text
          if(pval){
            enum = ceiling(log10(p.value)-1)
            fnum = round(p.value*10^abs(enum),2)
            text((x2+x1)/2,y2+len,labels=paste("P=",fnum,'e',enum,sep=""),font=3,cex=0.8)		 
          }else{
            if(pval<0.001){
              text((x2+x1)/2,y2+len,labels="***",font=3,cex=0.8)
            }else
              if(pval<0.01){
                text((x2+x1)/2,y2+len,labels="**",font=3,cex=0.8)
              }else{
                text((x2+x1)/2,y2+len,labels="*",font=3,cex=0.8)
              }
          }
        }   
      }
    }
  }
  
  #2
  par(mar = c(5,0,5,5))
  xn = ceiling(ng/10)
  if(xn>1){
    plot(0, 0, type='l', xlim = c(0,xn), ylim = c(0,10+1.5), xlab='', ylab='', axes = FALSE)  
    for(i in 1:xn){
      for(j in 1:10){
        if(j+10*(i-1)<=ng){
          points(0.05+1*(i-1),j,col=col[i],pch=16,cex=2/xn)
          text(0.1+1*(i-1), j,labels=ngg[i],col=text.col[i],adj=text.adj, cex=text.cex, font=text.font)            
        }     
      }        
    }
  }else{
    plot(0, 0, type='l', xlim = c(0,xn), ylim = c(0,ng+1.5), xlab='', ylab='', axes = FALSE)  
    if(length(unique(col))==1){
      ng = 1
      ngg = 'Person boxplot'
    }
    tmp.counts = rnorm(100,mean=0,sd=1)*0.3/1.8
    for(i in 1:ng){
      y1 = i
      counts = i + tmp.counts
      box.p(0.3,counts,0.3,col[i],fill,border,lwd,whisker.lty)
      text(0.5, y1,labels=ngg[i],col=text.col[i],adj=text.adj, cex=text.cex, font=text.font)       
    }
    if(length(unique(points.pch))==1){
      if(points.add){
		# 2019.07.23
		#################
		a = points.pch[1]
		if(!is.na(as.numeric(a))){
           a = as.numeric(a)
        }
		points(0.3,0.2, pch=a, bg=points.col[1], col=points.col[1], cex=1.2)
        #################		
        #points(0.3,0.2, pch=points.pch[1], bg=points.col[1], col=points.col[1], cex=1.2) # 2019.07.23
        text(0.5,0.2,labels ="Sample")  
      }   
    }
    if(!pval){
      text(0.5,ng+1,labels="***  P<0.001",font=4,cex=0.8,adj=0.5)
      text(0.5,ng+0.8,labels="**  P<0.01",font=4,cex=0.8,adj=0.5)
      text(0.5,ng+0.6,labels="*  P<0.05",font=4,cex=0.8,adj=0.5)
      segments(0,ng+0.5,1,ng+0.5,lwd=lwd/2,col='black')
      segments(0,ng+0.5,0,ng+1.1,lwd=lwd/2,col='black')
      segments(1,ng+0.5,1,ng+1.1,lwd=lwd/2,col='black')
      segments(0,ng+1.1,1,ng+1.1,lwd=lwd/2,col='black')  
    } 	
  }
}   
