\name{LexisDiagram-package}
\alias{LexisDiagram-package}
\alias{LexisDiagram}
\docType{package}
\title{Lexis diagram drawing made easy!}
\description{
A function for drawing Lexis diagrams (just specify ages and years), plus a triangle highlighting function. 
}
\details{
\tabular{ll}{
Package: \tab LexisDiagram\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2010-12-17\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
Contains two functions: \code{Lexis()} and \code{Thighlight()}, both very straightforward. Try the short examples and you'll get it. Please don't be bashful about suggestions and feedback.
}
\author{
Tim Riffe

Maintainer: Tim Riffe <tim.riffe@gmail.com>
}

\keyword{ package }

\examples{
ages <- 0:8
years <- 2000:2003
Lexis(ages=ages,years=years,labs=T)
Thighlight(2,col="#FF000050") # click on the graph to highlight 2 triangles
}
