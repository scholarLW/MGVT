\name{metaNMDS}
\alias{metaNMDS}

\title{
	metaNMDS.
}
\description{
	Function 'metaNMDS' performs Nonmetric Multidimensional Scaling (NMDS), and tries to find a stable solution using several random starts.
}
\usage{
    metaNMDS(file, groupFile=NULL, matrix.transpose=TRUE, distance.method='bray', k.cluster=NULL, try=20, trymax=20, ...)
}
\arguments{
  \item{file}{
	a file, its content is (N+1) x (D+1) matrix or data frame (rows: N samples; columns: D features;  The first column must be SampleID); 
	or a matrix, a array or data frame, as least N x (D+2) (rows: N samples; columns: D features + "Group" and "SampleID"). 	
	this parameter must be set.
}
  \item{groupFile}{
	a file, its content is as least (N+1) x 2 matrix or data frame (rows: N samples; columns: Group	SampleID) with one header. The SampleID must be consistent with the row names of N x D matrix or data frame. 
}
  \item{matrix.transpose}{
	if file is a file and its content is a (D+1) x (N+1) matrix, this parameter can be set to transpose this matrix and get a N x D matrix.
}
  \item{distance.method}{
	dissimilarity index, partial match to '"manhattan"', '"euclidean"', '"canberra"', '"clark"', '"bray"', '"kulczynski"', '"jaccard"', '"gower"', '"altGower"','"morisita"', '"horn"', '"mountford"', '"raup"', '"binomial"', '"chao"', '"cao"' or '"mahalanobis"'.
}
  \item{k.cluster}{
	number of dimensions.  NB., the number of points n should be n > 2*k + 1, and preferably higher in global non-metric MDS, and still higher in local NMDS.
}
  \item{z}{
	the third dimension.
}
  \item{try, trymax}{
	minimum and maximum numbers of random starts in search of stable solution. After 'try' has been reached, the iteration will stop when two convergent solutions were found or 'trymax' was reached.
}
  \item{stressplot,MDSplot}{
	scatter diagram of stress or NMDS. Note: MDSplot = !stressplot.
}
  \item{...}{
	arguments passed to dataPC.plot function.
}
}
\details{
 
}
\author{
%%  ~~who you are~~
Teng Xiong(design), Wen Luo(implementation, testing)
}
\examples{
data(PC)
data <- data[,1:279]
nmds1 <- metaNMDS(data,MDSplot=FALSE)
title <- paste('Stress =', round(nmds1$stress, 4))
metaNMDS(data,xlab='NMDS axis1',ylab='NMDS axis2',main=title)
metaNMDS(data,stressplot=TRUE)
metaNMDS(data,xlab='NMDS axis1',ylab='NMDS axis2',main=title,axes=FALSE,ellipse=TRUE)
}
