\name{metaCCA}
\alias{metaCCA}
\alias{RDA}
\alias{CCA}

\title{
	Visualization toolkit for Metagenome analysis.
}
\description{
	CCA analysis for meta data.
}
\usage{
    metaCCA(file, groupFile=NULL, matrix.transpose=TRUE, method=c("RDA","CCA"), personal=FALSE, trait.add=TRUE, envfit=TRUE, pvalue=0.05, p.adjust.method='fdr', qvalue=0.1, envfit.pvaluechoose=TRUE, plot=FALSE, ...)
}
\arguments{
   \item{file}{
	a file, its content is (N+1) x (D+1) matrix or data frame (rows: N samples; columns: D features;  The first column must be SampleID); 
	or a matrix, a array or data frame, then its columns are in the form of "SampleID,features 1 ~ n,HostID,Group,factors 1 ~ m". 	
	this parameter must be set.
}
  \item{groupFile}{
	a phenotype file, its content is as least (N+1) x D matrix or data frame (rows: N samples; columns: SampleID,HostID,Group,factors 1 ~ D-3) with one header;
	or a matrix, a array or data frame, then its columns are in the form of "SampleID,HostID,Group,factors 1 ~ D-3".
	The SampleID must be consistent with the row names of N x D matrix or data frame. 
}
  \item{matrix.transpose}{
	if file is a file and its content is a (D+1) x (N+1) matrix, this parameter can be set to transpose this matrix and get a N x D matrix.
}
  \item{method}{
	analyzing method, including CCA and RDA.
}
  \item{personal}{
	combining those data form the same individual.
}
  \item{trait.add}{
	considering features as factors or phenotypes.
}
  \item{envfit}{
	factors training and screening.
}
  \item{pvalue,p.adjust.method,qvalue}{
	p value and q value of testing. p.adjust.method:"holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr", "none".
}
  \item{envfit.pvaluechoose}{
    removing those not significant factors.
}  
  \item{...}{
	arguments passed to metaCCA.plot function.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Yusheng Deng(design); 
Wen Luo(implementation, testing)
}
\examples{
# 1
data(RDACCAplot)
file = res$OTU
groupFile = res$phenotype
data = metaCCA(file,groupFile)
data
}
