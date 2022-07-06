\name{veganPCoA}
\alias{veganPCoA}

\title{
	Visualization toolkit for Metagenome analysis.
}
\description{
	A collection of analysis toolkit for metagenome databases.
}
\usage{
    veganPCoA(file, groupFile=NULL, matrix.transpose=TRUE, vegdist.method='bray', k.cluster=NULL, na.rm=TRUE,z=FALSE)
}
\arguments{
  \item{file}{
	a file, its content is (N+1) x (D+1) matrix or data frame (rows: N samples; columns: D features;  The first column must be SampleID); 
	or a matrix, a array or data frame, as least N x (D+2) (rows: N samples; columns: D features + "HostID" and "SampleID";All columns can be "HostID","SampleID","Group","Groupcolor","Timeseries"). 	
	this parameter must be set.
}
  \item{groupFile}{
	a file, its content is as least (N+1) x 2 matrix or data frame (rows: N samples; columns: HostID	SampleID) with one header. The SampleID must be consistent with the row names of N x D matrix or data frame. All columns can be "HostID","SampleID","Group","Groupcolor","Timeseries".
}
  \item{matrix.transpose}{
	if file is a file and its content is a (D+1) x (N+1) matrix, this parameter can be set to transpose this matrix and get a N x D matrix.
}
  \item{vegdist.method}{
	dissimilarity index, partial match to '"manhattan"', '"euclidean"', '"canberra"', '"clark"', '"bray"', '"kulczynski"', '"jaccard"', '"gower"', '"altGower"','"morisita"', '"horn"', '"mountford"', '"raup"', '"binomial"', '"chao"', '"cao"' or '"mahalanobis"'.
}
  \item{k.cluster}{
	the maximum dimension of the space which the data are to be represented in; must be in {1, 2, ..., N-1}.
}
  \item{na.rm}{
	pairwise deletion of missing observations when computing dissimilarities.
}
  \item{z}{
	the third dimension.
}
}
\details{
     Multidimensional scaling takes a set of dissimilarities and returns a set of points such that the distances between the points are approximately equal to the dissimilarities.  (It is a major part of what ecologists call 'ordination'.)

     A set of Euclidean distances on n points can be represented exactly in at most n - 1 dimensions.  'cmdscale' follows the analysis of Mardia (1978), and returns the best-fitting k-dimensional representation, where k may be less than the argument 'k'.
	 
	 "Timeseries" represents the time series sample flag of the same host,and the specification uses Arabic numerals to indicate its sequence of sorting.
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}
\examples{

data(PC)
loc <- veganPCoA(data)
loc
}
