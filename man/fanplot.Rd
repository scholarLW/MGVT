\name{fanplot}
\alias{fanplot}

\title{
	fanplot function.
}
\description{
	A fan plot functions.
}
\usage{
    fanplot(x,edges=200,col=NULL,...)
}
\arguments{
  \item{x}{
	vector of numbers..
}
  \item{edges}{
	the number of edges with which to draw a circle.
}
  \item{col}{
	color code or name.
}
  \item{...}{
	further parameters of fan.plot in plotrix.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}
\examples{
iucn.df <- data.frame(area=c("Africa","Asia","Europe","N&C America","S America","Oceania"),threatened=c(5994,7737,1987,4716,5097,2093))
fanplot(iucn.df$threatened,max.span=pi,labels=paste(iucn.df$area,iucn.df$threatened,sep="-"),main="Threatened species by geographical area (fan.plot)",ticks=276)
}
