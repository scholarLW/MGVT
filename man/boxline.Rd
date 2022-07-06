\name{boxline}
\alias{boxline}

\title{
	boxplot function.
}
\description{
	box-whisker plot function.
}
\usage{
    boxline(data, col=NULL, fill=FALSE, border=NA, lwd=2, whisker.lty=2, alpha=150, lim=NULL, lab=NULL, main=NULL,
                    line.add=TRUE, line.col=NULL, line.lty=NULL, line.lwd=0.5, 
                    points.add=TRUE, points.pch=NULL, points.col=NULL, points.bg=points.col, points.cex=0.7, points.random=!line.add,
                    text.adj=0, text.cex=1, text.col=NULL, text.font=NULL,                     
                    t.interGroup=!line.add, t.intraGroup=!t.interGroup, t.method=c('Student','Wilcoxon'), pval=FALSE, ...)
}
\arguments{
  \item{data}{
	a matrix, a array or data frame, as least N x (D+1) (rows: N hosts; columns: D columns(>=1) + "HostID";All columns can be: "HostID Group sample1-value  sample2-value ... sampleN-value", and HostID column must require, sample1-value  sample2-value ... are the time series samples of host). 	
	this parameter must be set.
}
  \item{col}{
	color code or color name for the groups.
}
  \item{fill}{
	the fill color of the box.
}
  \item{border}{
	the color to draw the border. The default, NULL, means to use par("fg"). Use border = NA to omit borders.
}
  \item{lwd}{
	graphical parameters as in par, possibly vectors. NA values in col cause the segment to be omitted.
}
  \item{whisker.lty}{
	line type of whisker.
}
  \item{alpha}{
	alpha values are coerced to integers in 0:255 and the result is computed most efficiently.	
}
  \item{lim}{
	the range of coordinate values. 
}
  \item{lab}{
	the axis labels.
}
  \item{main}{
	the theme of the layout. 
}
  \item{line.add}{
	line for time series samples. 
}
  \item{line.col}{
	color of line.
}
  \item{line.lty}{
	type of line.
}
  \item{line.lwd}{
	thickness of line.
}
  \item{points.add}{
	mark point set.
}
  \item{points.pch}{
	this can either be a single character or an integer code for one of a set of graphics symbols.
}
  \item{points.col}{
	color code or name.
}
  \item{points.bg}{
	background (fill) color for the open plot symbols given by pch = 21:25.
}
  \item{points.cex}{
	character (or symbol) expansion: a numerical vector.
}
  \item{points.random}{
	Random distribution of point sets.
}
  \item{text.adj}{
	one or two values in [0, 1] which specify the x (and optionally y) adjustment of the labels, with 0 for left/bottom, 1 for right/top, and 0.5 for centered. On most devices values outside [0, 1] will also work.
}
  \item{text.cex}{
	numeric character expansion factor.
}
  \item{text.col}{
	the color to be used, possibly vectors.
}
  \item{text.font}{
	the font to be used, possibly vectors.
}
  \item{t.interGroup}{
    group comparison.
}
  \item{t.intraGroup}{
	in-group comparison.
}
  \item{ t.method}{
	statistical testing method, such as 'Student' and 'Wilcoxon'.
}

  \item{pval}{
	show p-value.
}
  \item{...}{
	arguments passed to statistical testing method.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}
\examples{
# 1
data(BMI)
boxline(BMI,lab='BMI(kg/m^2)',col=c('#07B8F0','#DE2FD9'),lwd=3)

# 2
data(BMI)
BMI = BMI[,1:3]
BMI$Group=c(rep('P',18),rep('PM',19),rep('M',17),rep('CON',21))
boxline(BMI,lab='BMI(kg/m^2)',lwd=3,line.add=FALSE,pval=TRUE)
}
