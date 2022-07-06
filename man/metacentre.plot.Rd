\name{metacentre.plot}
\alias{metacentre.plot}

\title{
	Visualization toolkit for Metagenome analysis.
}
\description{
	Draw function of CCA analysis for meta dataset.
}
\usage{
    metacentre.plot(x, y=NULL, centre=FALSE, sd=TRUE, xlab='PC1', ylab='PC2', main='Principal coordinate analysis', xlim=NULL, ylim=NULL,
                           alpha=NA, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=2, 
                           add.labels=TRUE, labels.font=1, labels.cex=1, labels.col='black', labels.adj=0.5,
                           level=0.95, line.lwd=3, line.lty=1, line.col="#7F7F7F", len=NA, mycolors=NULL,
                           addEllipse=!centre, addPolygon=addP(centre,addEllipse), ...)
}
\arguments{
  \item{x}{
	a N x 4 matrix or data frame (rows: N samples; 4 columns: "SampleID,Group,Dim1,Dim2"). 	
	this parameter must be set.
}
  \item{y}{
	a N x 3,4,5 matrix or data frame (rows: N samples; 3~5 columns: "Variables,Dim1,Dim2,[R2,Pvalue]"). 
}
  \item{centre}{
	Only draw centers of groups.
}
  \item{sd}{
	standard deviation.
}
  \item{xlab,ylab,main,xlim,ylim}{
	plot parameters.
}
  \item{alpha}{
	alpha values are coerced to integers in 0:255 and the result is computed most efficiently.	
}
  \item{points.pch}{
	this can either be a single character or an integer code for one of a set of graphics symbols.
}
  \item{points.col}{
	color code or name.
}
  \item{points.bg}{
	background (fill) color for the open plot symbols given by pch = 21:25.
}
  \item{points.cex}{
	character (or symbol) expansion: a numerical vector.
}
  \item{add.labels}{
	add labels.
}
  \item{labels.font}{
     the font to be used, possibly vectors. 
}
  \item{labels.cex}{
    numeric *c*haracter *ex*pansion factor; multiplied by 'par("cex")' yields the final character size.
}
  \item{labels.col}{
   color code or name, see 'par'.
}
  \item{labels.adj}{
   one or two values in [0, 1] which specify the x (and optionally y) adjustment of the labels.  On most devices values outside that interval will also work.
}
  \item{level}{
	confidence level of the interval.
}
  \item{line.lwd}{
   line width for drawing symbols see 'par'.
}
  \item{line.lty}{
	further graphical parameters (see 'par') may also be supplied as arguments, particularly, line type.
}
  \item{line.col}{
   color code or name, see 'par'.
}
  \item{len}{
   length of the line on the endpoint of confidence interval.
}
  \item{mycolors}{
   color code or name of color stripes, a vector.
}
  \item{addEllipse}{
	draw ellipse. 
}
  \item{addPolygon}{
	draw polygon. 
}
  \item{...}{
	arguments passed to Ellipse or Polygon function. 
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
data(RDACCAplot)
file = res$OTU
groupFile = res$phenotype
data = metaCCA(file,groupFile)
x = as.data.frame(cbind(as.character(data$data$SampleID),as.character(data$data$Group),data$sp1$CCA$u[,1:2]))
colnames(x) = c('SampleID','Group','Dim1','Dim2')
y = as.data.frame(cbind(rownames(data$ef$vectors$arrows),data$ef$vectors$arrows,data$ef$vectors$r,data$ef$vectors$pvals))
colnames(y) = c('Variables','Dim1','Dim2','R2','Pvalue')
metacentre.plot(x,y,centre=TRUE)
}
