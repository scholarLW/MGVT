\name{arrows}
\alias{arrows}

\title{
	arrows function.
}
\description{
	A arrow functions.
}
\usage{
    arrows(x, y=NULL, group=NULL, col=NULL, lwd=1, arr.length=0.2, lty=1, distance=10^-3, arr.gap.error=10^-3,
                   plot=FALSE, plot.points=plot, points.pch=NULL, points.col=col, points.bg=points.col, points.cex=1,
                   plot.text=plot, text.adj = 0, text.cex = 1, text.col = col, text.font = NULL, ...)
}
\arguments{
  \item{x}{
	a N x 2 matrix or data frame, or a vector with a length of N.
}
  \item{y}{
	a vector with a length of N.
}
  \item{group}{
	a vector with a length of N.
}
  \item{col}{
	color code or name of groups.
}
  \item{lwd}{
	graphical parameters as in par, possibly vectors. NA values in col cause the segment to be omitted.
}
  \item{arr.length}{
	approximate length of arrowhead, in cm; either one value or a vector.
}
  \item{lty}{
	line type specifications; either one value or a vector.
}
  \item{distance}{
	the distance of the arrow to the coordinate point.
}
  \item{arr.gap.error}{
	the true distance between the estimate point and the arrow pointing point is the wrong distance from the distance.
}
  \item{plot}{
	plot.
}
  \item{plot.points}{
	mark point set.
}
  \item{points.pch}{
	this can either be a single character or an integer code for one of a set of graphics symbols.
}
  \item{points.col}{
	color code or name.
}
  \item{points.bg}{
	background color for the open plot symbols given by pch = 21:25.
}
  \item{points.cex}{
	a numerical vector.
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
data(PCoAplot) #PRJEB17643
x = res$points
cons = res$cons
x = x[order(x$Timeseries),c('x','y','HostID')]
group = as.character(x[,3])
x = x[,-3]
arrows(x,group=group,plot=TRUE,xlab=paste('PC1(',cons[1],'\%)',sep=""),ylab=paste('PC2(',cons[2],'\%)',sep=""),main=c('PCoA of metagenomic species'))
}
