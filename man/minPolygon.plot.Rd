\name{minPolygon.plot}
\alias{minPolygon.plot}

\title{
	Drawing function of principal component.
}
\description{
	Drawing function of principal component,such as PCoA, PCA, tSNE and so on.
}
\usage{
    minPolygon.plot(points=NULL, fill=FALSE, col=NULL, lwd=2, lty=1, border=NA, alpha=150, plot=TRUE, addPoints=TRUE,
                            axis.scale=NULL, ayis.scale=NULL, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=1, 
                            text.adj = 0, text.cex = 1, text.col = NULL, text.font = NULL, axes=TRUE, ...)
}
\arguments{
  \item{points}{
	a matrix, a array or data frame, as least N x (D+2) (rows: N samples; columns: D columns(2) + "x" and "y";All columns must be "SampleID","Group","x","y"). 	
	this parameter must be set.
}
  \item{fill}{
	the fill color of the polygon.
}
  \item{col}{
	color code or name of groups.
}
  \item{lwd}{
	graphical parameters as in par, possibly vectors. NA values in col cause the segment to be omitted.
}
  \item{lty}{
	Further graphical parameters (see 'par') may also be supplied as arguments, particularly, line type of polygon.
}
  \item{border}{
	the color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.
}
  \item{alpha}{
	alpha values are coerced to integers in 0:255 and the result is computed most efficiently.	
}
  \item{plot}{
	plot.
}
  \item{addPoints}{
	draw points.
}
  \item{axis.scale}{
	X-axis scale value
}
  \item{ayis.scale}{
	Y-axis scale value
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
  \item{plot.text}{
	text labels.
}
  \item{text.adj}{
	one or two values in [0, 1] which specify the x (and optionally y) adjustment of the labels, with 0 for left/bottom, 1 for right/top, and 0.5 for centered. On most devices values outside [0, 1] will also work.
}
  \item{text.cex}{
	numeric character expansion factor.
}
  \item{text.col}{
	the color to be used, possibly vectors.
}
  \item{text.font}{
	the font to be used, possibly vectors.
}
  \item{axes}{
	coordinate axis.
}
  \item{...}{
	arguments passed to plot function.
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
data(PCoAplot) #PRJEB17643
points = res$points
cons = res$cons
minPolygon.plot(points, lwd=2, fill=TRUE, ylab=paste("PC2 (",cons[2],'\%)',sep=""), xlab=paste("PC1 (",cons[1],'\%)',sep=""), main=c('PCoA of metagenomic species'))
}
