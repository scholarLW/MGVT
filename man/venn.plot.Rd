\name{venn.plot}
\alias{venn.plot}

\title{
	venn.plot function.
}
\description{
	A venn plot functions.
}
\usage{
    venn.plot(x,...)
}
\arguments{
  \item{x}{
	a list of vectors (e.g., integers, chars), with each component corresponding to a separate circle in the Venn diagram.
}
  \item{...}{
	further parameters of venn.diagram in VennDiagram.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}
\examples{
A = 1:150
B = c(121:170,300:320)
venn.plot(list(A=A,B=B),filename=NULL,lwd=1,lty=2,col=c('red','green'),fill=c('red','green'),cat.col=c('red','green'),rotation.degree=90)
}
