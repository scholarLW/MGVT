fanplot = function(x,edges=200,col=NULL,...){     
     require(plotrix)
     if(is.null(col)){               
       col = Colorpools()[1:length(x)]  
     } 	 
     plotrix::fan.plot(x,edges=edges,col=col,...)
}

venn.plot = function(x,...){
     require(VennDiagram)     
     p <- venn.diagram(x,...)
     grid.draw(p)
}

heatmap.plot = function(x,col="heat.colors",...){
     require(gplots)     	 
     heatmap.2(x,col=col,...)
}