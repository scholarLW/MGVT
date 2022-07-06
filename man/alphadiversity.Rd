\name{alphadiversity}
\alias{alphadiversity}
\alias{alpha}
\alias{Alpha}

\title{
	Visualization toolkit for Metagenome analysis.
}
\description{
	A collection of analysis toolkit for metagenome databases.
}
\usage{
    alphadiversity(file, groupFile=NULL, matrix.transpose=TRUE, tab.readcounts=FALSE, 
                          plot=FALSE, select.plot=c('Shannon','Simpson','Chao1','S.obs','S.chao1','se.chao1','S.ACE','se.ACE'), ...)
}
\arguments{
  \item{file}{
	a file, its content is (N+1) x (D+1) matrix or data frame (rows: N samples; columns: D features;  The first column must be SampleID); 
	or a matrix, a array or data frame, as least N x (D+2) (rows: N samples; columns: D features + "HostID" and "SampleID";All columns can be "HostID","SampleID","Group", and so on.). 	
	this parameter must be set.
}
  \item{groupFile}{
	a file, its content is as least (N+1) x 2 matrix or data frame (rows: N samples; columns: HostID	SampleID) with one header. The sampleID must be consistent with the row names of N x D matrix or data frame. All columns can be "HostID","SampleID","Group",and so on.
}
  \item{matrix.transpose}{
	if file is a file and its content is a (D+1) x (N+1) matrix, this parameter can be set to transpose this matrix and get a N x D matrix.
}
  \item{plot,select.plot}{
	plot or not. the length of select.plot must be 1.
}
  \item{...}{
	arguments passed to boxline.
}
}
 
\author{
%%  ~~who you are~~
Yusheng Deng(design)
Wen Luo(implementation, testing)
}
\examples{

data(OTU)
loc <- alphadiversity(OTU)
loc
}
