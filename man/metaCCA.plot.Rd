\name{metaCCA.plot}
\alias{metaCCA.plot}

\title{
	Visualization toolkit for Metagenome analysis.
}
\description{
	Draw function of CCA analysis for meta dataset.
}
\usage{
    metaCCA.plot(x, group=NULL, addEllipse=TRUE, addPolygon=!addEllipse, fill=TRUE, ellipse.method='car', car.level=0.95, col=NULL, lty=1, border=NA, alpha=150, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=1)
}
\arguments{
  \item{x}{
	a object from CCA analysis, for examples: x <- cca(tmp~.,envi);
	or a list call from metaCCA function.
}
  \item{group}{
	a vector with a length of N.
}
  \item{addEllipse}{
	draw ellipse. 
}
  \item{addPolygon}{
	draw polygon. 
}
  \item{fill}{
	the fill color of the ellipse.
}
  \item{ellipse.method}{
	method to generate an ellipse from R package, such as Funset,car.
}
  \item{car.level}{
	confidence level of confidence ellipse.
}
  \item{col}{
	color code or name of groups.
}
  \item{lty}{
	Further graphical parameters (see 'par') may also be supplied as arguments, particularly, line type of ellipse.
}
  \item{border}{
	the color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.
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
metaCCA.plot(data)

}
