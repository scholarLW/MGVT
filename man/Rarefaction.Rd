\name{Rarefaction}
\alias{Rarefaction}

\title{
	Drawing function of Rarefaction analysis.
}
\description{
	Drawing function of Rarefaction analysis.
}
\usage{
    Rarefaction <- function(file, matrix.transpose=TRUE, method='random', permutations=100, conditioned=TRUE, box.col='yellow', outliers.pch="+", 
                          plot.col="blue", ci.type="poly", lwd=2, ci.lty=0, ci.col="lightblue", ci.alpha=150, ...)
}
\arguments{
    \item{file}{
	a file, its content is (N+1) x (D+1) matrix or data frame (rows: N samples; columns: D features;  The first column must be SampleID); 
	or a matrix, a array or data frame, as least N x D (rows: N samples; columns: D features). 	
	this parameter must be set.
}
  \item{matrix.transpose}{
	if file is a file and its content is a (D+1) x (N+1) matrix, this parameter can be set to transpose this matrix and get a N x D matrix.
}
  \item{method}{
	Species accumulation method (partial match). Method "collector" adds sites in the order they happen to be in the data, "random" adds sites in random order, "exact" finds the expected (mean) species richness, "coleman" finds the expected richness following Coleman et al. 1982, and "rarefaction" finds the mean when accumulating individuals instead of sites.
}
  \item{permutations}{
	Number of permutations with method = "random". Usually an integer giving the number permutations, but can also be a list of control values for the permutations as returned by the function how, or a permutation matrix where each row gives the permuted indices.
}
  \item{conditioned}{
	Estimation of standard deviation is conditional on the empirical dataset for the exact SAC
}
  \item{box.col}{
	if box.col is non-null it is assumed to contain colors to be used to colour the bodies of the box plots. 
}
  \item{outliers.pch}{
	this can either be a single character or an integer code for one of a set of graphics symbols.
}
  \item{plot.col}{
	colour for drawing lines.
}
  \item{ci.type}{
	type of confidence intervals in the graph: "bar" draws vertical bars, "line" draws lines, and "polygon" draws a shaded area.
}
  \item{lwd}{
	graphical parameters as in par, possibly vectors. NA values in col cause the segment to be omitted.
}
  \item{ci.lty}{
	line type.
}
  \item{ci.col}{
	the color of confidence intervals in the graph: "bar" draws vertical bars, "line" draws lines, and "polygon" draws a shaded area..
}
  \item{ci.alpha}{
	alpha values are coerced to integers in 0:255 and the result is computed most efficiently.	
}
  \item{...}{
	arguments passed to plot function.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Teng Xiong(design), Wen Luo(implementation, testing)
}
\examples{
library(vegan)
data(BCI)
Rarefaction(BCI,matrix.transpose=FALSE,xlab='SampleNumber',ylab='Species',main='Rarefaction Analysis')
}
