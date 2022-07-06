\name{ellipse}
\alias{ellipse}

\title{
	ellipse function.
}
\description{
	A ellipse functions.
}
\usage{
    ellipse(x, y=NULL, group=NULL, method=c('Funset','car'), col=NULL, lwd=1, lty=1, fill=FALSE, border=NA, alpha=150,
                    plot=FALSE, axis.scale=NULL, ayis.scale=NULL, axes=TRUE, xlab='', ylab='',main='',
                    plot.points=plot, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=1,
                    plot.text=plot, text.adj = 0, text.cex = 1, text.col = NULL, text.font = NULL, ...)
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
  \item{method}{
	method to generate an ellipse from R package, such as Funset,car.
}
  \item{col}{
	color code or name of groups.
}
  \item{lwd}{
	graphical parameters as in par, possibly vectors. NA values in col cause the segment to be omitted.
}
  \item{lty}{
	Further graphical parameters (see 'par') may also be supplied as arguments, particularly, line type of ellipse.
}
  \item{fill}{
	the fill color of the ellipse.
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
  \item{axis.scale}{
	X-axis scale value
}
  \item{ayis.scale}{
	Y-axis scale value
}
  \item{axes}{
	coordinate axis.
}
  \item{xlab}{
    x-axis annotation.
}
  \item{ylab}{
	y-axis annotation.
}
  \item{main}{
    an overall title for the plot.
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
  \item{...}{
	arguments passed to generate an ellipse from R package.
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
ellipse(x,group=group,method='Funset',plot=TRUE,xlab=paste('PC1(',cons[1],'\%)',sep=""),ylab=paste('PC2(',cons[2],'\%)',sep=""),main=c('PCoA of metagenomic species'),fill=TRUE)
ellipse(x,group=group,method='Funset',plot=TRUE,xlab=paste('PC1(',cons[1],'\%)',sep=""),ylab=paste('PC2(',cons[2],'\%)',sep=""),main=c('PCoA of metagenomic species'),fill=FALSE)

}
