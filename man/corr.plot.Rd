\name{corr.plot}
\alias{corr.plot}

\title{
	Visualization toolkit for Metagenome analysis.
}
\description{
	Draw function of CCA analysis for meta dataset.
}
\usage{
    corr.plot(x, matrix.transpose = FALSE, cor.method = c("pearson", "kendall", "spearman"), method = c("circle", "square", "ellipse", "number", "shade", "color", "pie"), type = c("full", "lower", "upper"), 
                     order = c("original","AOE", "FPC", "hclust", "alphabet"), hclust.method = c("complete", "ward", "ward.D", "ward.D2", "single", "average", "mcquitty", "median", "centroid"),
                     col = NULL, tl.cex = 0.8, tl.col = "black", addshade = c("negative", "positive", "all"), shade.lwd = 0.1, shade.col = NA, ...)
}
\arguments{
  \item{x}{
	a file with colnames and rownames;
	or a matrix or data frame (rows: samples; columns: features). 	
	this parameter must be set.
}
  \item{matrix.transpose}{
	if file is a file and its content is a (D+1) x (N+1) matrix, this parameter can be set to transpose this matrix and get a N x D matrix.
}
  \item{cor.method}{
	a character string indicating which correlation coefficient (or covariance) is to be computed. One of "pearson" (default), "kendall", or "spearman": can be abbreviated.
}
  \item{method,type,order,hclust.method,col,tl.cex,tl.col,addshade,shade.lwd,shade.col,...}{
	parameters of corrplot.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Le liu(design), Wen Luo(implementation, testing)
}
\examples{
# 1
data(corr.plot)
corr.plot(data)
}
