\name{heatmap.plot}
\alias{heatmap.plot}

\title{
	heatmap.plot function.
}
\description{
	A heatmap plot functions.
}
\usage{
    heatmap.plot(x,col="heat.colors",...)
}
\arguments{
  \item{x}{
	a matrix or data frame, columns.
}
  \item{col}{
	colors used for the image. Defaults to heat colors (heat.colors).
}
  \item{...}{
	further parameters of heatmap.2 in gplots.
}
}
\details{
    
}
\author{
%%  ~~who you are~~
Wen Luo(design, implementation, testing)
}
\examples{
library(ALL)
data(ALL)
eset <- ALL[, ALL$mol.biol %in% c("BCR/ABL", "ALL1/AF4")]
library(limma)
f <- factor(as.character(eset$mol.biol))
design <- model.matrix(~f)
fit <- eBayes(lmFit(eset,design))
selected  <- p.adjust(fit$p.value[, 2]) < 0.05
esetSel <- eset[selected,]
x = exprs(esetSel)
#> x[1:3,1:4]
#             01005    03002    04006    08001
#1007_s_at 7.623562 7.916954 6.816397 7.543907
#1039_s_at 8.063428 8.126621 5.739973 8.126982
#1126_s_at 6.875240 6.730446 9.338491 7.826722
heatmap.plot(x)
}
