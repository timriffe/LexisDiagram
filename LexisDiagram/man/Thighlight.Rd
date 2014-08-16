\name{Thighlight}
\alias{Thighlight}

\title{
A function to highlight triangles on the diagram drawn by \code{Lexis}.
}
\description{
Just specify how many you want to click and what color they should be. Then click the diagram where you want triangles highlighted.
}
\usage{
Thighlight(n = 1, col = "#00206050")
}

\arguments{
  \item{n}{
a number. How many triangles will you want to be clicking, sir?
}
  \item{col}{
The color to be used. Specify in any way accepted by plot colors. If you want opacity, I suggest the HEX RGB specification per the example. These are character strings starting with #, followed by 6 numbers RRGGBB, then two more for opacity e.g. "#RRGGBB50" for 50%.
}
}
\details{
try the examples. Bear in mind you can still manually plot over the Lexis diagram if this function doesn't dowhat you want. 
}
\value{
performs action on the Lxis diagram in the active graphics device.
}


\author{
Tim Riffe
}
\note{
Bear in mind that you need to have specified years and ages as named objects. If you specified them directly in the arguments for \code{Lexis()}, then this function will not know where they are and it won't work. Sorry for this quirk.
}

\seealso{
%% ~~objects to See Also as \code{\link{Lexis}}, ~~~
}
\examples{
ages <- 20:25
years <- 2000:2004
Lexis(ages=ages,years=years,labs=T)
Thighlight(1,"yellow")

## The function is currently defined as
function(n=1,col="#00206050"){
  for (i in 1:n){
    xy <- locator(1,type="n")
    lft <- max(years[years<xy$x]);rght <- min(years[years>xy$x])
    tp <- min(ages[ages>xy$y]);btm <- max(ages[ages<xy$y])
    if ((xy$x-lft)>(xy$y-btm))
        polygon(c(lft,rght,rght),c(btm,btm,tp),col=col)
    else
        polygon(c(lft,lft,rght),c(btm,tp,tp),col=col)}
  }
}

\keyword{iplot}
\keyword{aplot}
