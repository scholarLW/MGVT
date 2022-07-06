\name{violin.plot}
\alias{violin.plot}

\title{
	violin.plot function.
}
\description{
	A violin plot functions.
}
\usage{
    violin.plot(x, group=NULL, coordinate.lab='Value', coordinate.lim=NULL, main='Violin plot', alpha=NA, col=NULL, rectCol=NULL, ...)
}
\arguments{
  \item{x}{
	a N x 2(3) matrix or data frame, columns: "SampleID,x" or "SampleID,Group,x".
}
  \item{group}{
	a vector with a length of N.
}
  \item{coordinate.lab}{
	coordinate annotation.
} 
  \item{coordinate.lim}{
	coordinate interval.
}
  \item{main}{
    an overall title for the plot.
}
  \item{alpha}{
	alpha values are coerced to integers in 0:255 and the result is computed most efficiently.	
}
  \item{col}{
	color code or name.
}
  \item{rectCol}{
	color code or name.	
}
  \item{...}{
	further parameters of vioplot, such as lty, lwd, lineCol, pchMed, colMed, colMed2, outer.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Jiao Xu(design), Wen Luo(implementation, testing)
}
\examples{

data(PCoAplot) #PRJEB17643
x = res$points
violin.plot(x)
}
