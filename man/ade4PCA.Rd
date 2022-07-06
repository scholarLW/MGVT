\name{ade4PCA}
\alias{ade4PCA}

\title{
	Visualization toolkit for Metagenome analysis.
}
\description{
	A collection of analysis toolkit for metagenome databases.
}
\usage{
    ade4PCA(file, groupFile=NULL, matrix.transpose=TRUE, vegdist.method='bray', k.cluster=NULL, na.rm=TRUE)
}
\arguments{
  \item{file}{
	a file, its content is (N+1) x (D+1) matrix or data frame (rows: N samples; columns: D features;  The first column must be SampleID); 
	or a matrix, a array or data frame, as least N x (D+2) (rows: N samples; columns: D features + "HostID" and "SampleID";All columns can be "HostID","SampleID","Group","Groupcolor","Timeseries"). 	
	this parameter must be set.
}
  \item{groupFile}{
	a file, its content is as least (N+1) x 2 matrix or data frame (rows: N samples; columns: HostID	SampleID) with one header. The sampleID must be consistent with the row names of N x D matrix or data frame. All columns can be "HostID","SampleID","Group","Groupcolor","Timeseries".
}
  \item{matrix.transpose}{
	if file is a file and its content is a (D+1) x (N+1) matrix, this parameter can be set to transpose this matrix and get a N x D matrix.
}
  \item{center}{
	a logical or numeric value, centring option
	if TRUE, centring by the mean
	if FALSE no centring
	if a numeric vector, its length must be equal to the number of columns of the data frame df and gives the decentring.
}
  \item{nf}{
	an integer indicating the number of kept axes.
}
  \item{z}{
	the third dimension.
}
}
\details{
     "Timeseries" represents the time series sample flag of the same host,and the specification uses Arabic numerals to indicate its sequence of sorting.
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}
\examples{

data(PC)
loc <- ade4PCA(data)
loc
}
