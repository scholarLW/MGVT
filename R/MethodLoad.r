# method from R package

## Funset R package
ellipse.Funset <- function(x,y,segments=1000){
  require(Funset)
  f <- elli.f(x,y)
  ellipse <- elli.points(f,segments=segments)
  return(ellipse)
}

## car R package
ellipse.car <- function(x, y, weights=NULL, level= 0.95, log="", center.pch=FALSE, center.cex=1.5, segments=100, draw=FALSE, add=draw, xlab="", ylab="", col=NULL, lwd=2, fill=FALSE, fill.alpha=0.3, grid=TRUE, ...){
  require(car)
  if(is.null(weights)){
    weights <- rep(1, length(x))
  }	
  v <- cov.wt(cbind(x, y), wt=weights)
  shape <- v$cov
  center <- v$center 
  dfn <- 2
  dfd <- length(x) - 1  
  radius <- sqrt(dfn * qf(level, dfn, dfd ))  # The F Distribution
  #radius <- sqrt(qchisq(level, 2)) # The (non-central) Chi-Squared Distribution
  data = car::ellipse(center, shape, radius, log=log,
            center.pch=center.pch, center.cex=center.cex, segments=segments, 
            col=carPalette()[2], lwd=lwd, fill=fill, fill.alpha=fill.alpha, draw=draw, ...)
  return(data)
}

# method to generate an ellipse from R package

## method list 
MethodList = function(){
  return(c(
    "Funset",   # Perimeter ellipse
    "car"       # Confidence ellipse
  ))
}

## invoking method from R package 
MethodLoad <- function (method,x,y,...) {
  switch(
    method,
    "Funset"  = ellipse.Funset(x,y,...),  
    "car"  =  ellipse.car(x,y,...)
  )
}
