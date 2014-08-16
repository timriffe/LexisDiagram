\name{Lexis}
\alias{Lexis}

\title{A function for drawing Lexis diagrams!}
\description{
A function for drawing Lexis diagrams! Specify an age and year range. Labels are optional. Also includes a triangle highlighting function that works by clicking.
}
\usage{
Lexis(ages, years, labs = T, col = "black")
}

\arguments{
  \item{ages}{a vector of age values. ex \code{c(15:25)}. Horizontal lines are drawn at these points.}
  \item{years}{
a vector of year values. ex \code{c(2000:2004)}. Vertical lines are drawn at these points.
}
  \item{labs}{logical. default = TRUE. Should age, year and generation labels be drawn in the margins? For large spans of years and ages it's best to specify FALSE.}
  \item{col}{The line color to be used. Default = "black".}
}
\details{Nothing fancy here. Please report bugs!}
\value{graphical device is activated with appropriate dimensions. Lexis diagram is drawn.}
\author{Tim Riffe}
\note{Great for presentation graphics and in the classroom!}


\seealso{See Also as \code{\link{Thighlight}}
}
\examples{
Lexis(ages=0:8,years=2000:2003,labs=T)
Lexis(ages=0:8,years=2000:2003,labs=F)

## The function is currently defined as

function(ages,years,labs=T,col="black"){
	xrange <- diff(range(years))
	yrange <- diff(range(ages))
	maxrange <- max(c(xrange,yrange))
	xrange <- xrange*(5/maxrange)
	yrange <- yrange*(5/maxrange)
	if (labs==T){
		# x coordinate for generations
		interval <- 5/max(c(length(ages),length(years)))
		genx <- max(years)+ .25/interval
		# year labels don't always fit:
		x <- years; y <- ages; xn <- length(x); yn <- length(y)
		windows(width=xrange+2,height=yrange+2)
		par(mai=c(1,1,1,1))
		plot(NA,type="n",xlim=c(min(x),max(x)),ylim=c(min(y),max(y)),axes=F,xlab="Years",ylab="Age",cex.lab=1.5,asp=1,col=col)
		segments(x,min(ages),x,max(ages),col=col);segments(min(years),y,max(years),y,col=col)
		for (i in 1:(xn-1)) {
			segments(rep(x[i],(xn-1)),y[-yn],rep(x[i+1],(xn-1)),y[-1],col=col)
			axis(1,at=x,labels=x,tick=F,pos=(min(y)-(.1/interval)))
			axis(2,at=y,labels=y,tick=F,pos=(min(x)),las=2)
			for (i in 1:yn){
				text(genx,y[i]+1,labels=(max(x)-y[i]-1),srt=45,xpd=T)
			}
			text(genx+(.5/interval),mean(ages),"Birth Cohort",srt=270,cex=1.5,xpd=T)
		}
	}
	if (labs==F){
    	windows(width=xrange,height=yrange)
		par(mai=c(0,0,0,0))
		x <- years; y <- ages; xn <- length(x); yn <- length(y)
		plot(NA,type="n",xlim=c(min(x),max(x)),ylim=c(min(y),max(y)),axes=F,xlab="",ylab="",cex.lab=1.5,asp=1,col=col)
		segments(x,min(ages),x,max(ages),col=col);segments(min(years),y,max(years),y,col=col)
		for (i in 1:(xn-1)) {
    		segments(rep(x[i],(xn-1)),y[-yn],rep(x[i+1],(xn-1)),y[-1],col=col)
    	}
	}
  }
}

\keyword{hplot}
\keyword{graphics}
\keyword{demography}
