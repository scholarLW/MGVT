\name{dataPC3D.plot}
\alias{dataPC3D.plot}

\title{
	Drawing function of principal component.
}
\description{
	2D-3D Drawing function of principal component,such as PCoA, PCA, tSNE and so on.
}
\usage{
    dataPC3D.plot(x=NULL, y=NULL, f.selectcolumn=1, D3=FALSE, D3.method=NULL, D3.col=NULL, D3.alpha=150, D3.pch=NULL, 
                          D2.selectcol=NULL, D2.axis.scale=NULL, D2.ayis.scale=NULL, D2.axes=TRUE, D2.sub.title=NULL,
                          cex=1, ...)
}
\arguments{
  \item{x}{
	a matrix, a array or data frame, as least N x D(D>=4) (rows: N samples; the first four columns: "SampleID", "Group", "x" and "y"). 	
	this parameter must be set.
}
  \item{y}{
	a matrix, a array or data frame, as least N x (1+D) (rows: N samples; columns: "SampleID" + D(>=1) columns, can be phenotype or abundance). 	
	this parameter must be set.
}
  \item{f.selectcolumn}{
	a feature from D for selecting to draw, should be positive integer: 1,2,3, ... . 
}
  \item{D3}{
	plot 3D figure.
}
  \item{D3.method}{
	3D plot method, such as "plot3D" and "scatterplot3d".
}
  \item{D3.col}{
	color code or name of groups.
}
  \item{D3.alpha}{
	alpha values are coerced to integers in 0:255 and the result is computed most efficiently.	
}
  \item{D3.pch}{
	this can either be a single character or an integer code for one of a set of graphics symbols.
}
  \item{D2.selectcol}{
	color set for 2D plot, there are two choices: 0 and 1.
}
  \item{D2.axis.scale}{
	X-axis scale value
}
  \item{D2.ayis.scale}{
	Y-axis scale value
}
  \item{D2.axes}{
	coordinate axis.
}
  \item{D2.axes}{
	coordinate axis.
}
  \item{D2.sub.title}{
	title of panel 2 for 2D plot.
}
  \item{...}{
	arguments passed to plot/plot3D/scatterplot3d function.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}
\examples{
# 1
data(PCoA.3D) 
x = res$x
y = res$y
cons = res$cons
dataPC3D.plot(x,y,ylab=paste("PC2 (",cons[2],'\%)',sep=""),xlab=paste("PC1 (",cons[1],'\%)',sep=""))

# 2
x = res$x
y = res$y
da = merge(x,y,by="SampleID",all=F)
ix = is.na(da$BMI)
da=da[!ix,]
da$Group=as.character(da$Group)
ix = da$BMI>=24
da$Group[ix]='BMI>=24'
da$Group[!ix]='BMI<24'
x=da[,c('SampleID','Group','x','y')]
y=da[,c(1,5:11)]
cons = res$cons
dataPC3D.plot(x,y,ylab=paste("PC2 (",cons[2],'\%)',sep=""),xlab=paste("PC1 (",cons[1],'\%)',sep=""),D3=TRUE,D3.pch = c(20,17),D3.alpha = 150,zlab='Abundance of Proteobacteria',f.selectcolumn = 3)
}
