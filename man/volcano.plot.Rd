\name{volcano.plot}
\alias{volcano.plot}

\title{
	volcano.plot function.
}
\description{
	A volcano plot functions.
}
\usage{
    volcano.plot(x, y=NULL, xlab='log2(FoldChange)', ylab='-log10(P-value)', main='Volcanno plot',
                           alpha=NA, pch=NULL, col=c('#FFCC66','#000000','#FF6666'), bg=col, cex=1, log2FCth=1, pvalueth=0.05,
						   line.col='grey', line.lwd=2, line.lty=2)
}
\arguments{
  \item{x}{
	a N x 2 matrix or data frame, or a vector with a length of N.
}
  \item{y}{
	a vector with a length of N.
}
  \item{xlab,ylab,main}{
	plot parameters.
}
  \item{alpha}{
	alpha values are coerced to integers in 0:255 and the result is computed most efficiently.	
}
  \item{pch}{
	this can either be a single character or an integer code for one of a set of graphics symbols.
}
  \item{col}{
	color code or name.
}
  \item{bg}{
	background (fill) color for the open plot symbols given by pch = 21:25.
}
  \item{cex}{
	character (or symbol) expansion: a numerical vector.
}
  \item{log2FCth}{
	threshold of Fold Change.
}
  \item{pvalueth}{
	threshold of P-value.
}
  \item{line.col}{
   color code or name, see 'par'.
}
  \item{line.lwd}{
   line width for drawing symbols see 'par'.
}
  \item{line.lty}{
	further graphical parameters (see 'par') may also be supplied as arguments, particularly, line type.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}
\examples{
data(volcano.plot)
x = data[,4:5]
volcano.plot(x)
}
