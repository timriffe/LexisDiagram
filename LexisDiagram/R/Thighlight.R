Thighlight <- function(n=1,col="#00206050",...){
  for (i in 1:n){
    xy 		<- locator(1,type="n")
	
#	# detect age and year ranges:
#	yl 		<- round(par()$xaxp[1:2])
#	al 		<- round(par()$yaxp[1:2])
#	# make selection vectors
#	years 	<- yl[1]:yl[2]
#	ages    <- al[1]:al[2]
	
#   lft 	<- max(years[years<xy$x])
#	rght 	<- min(years[years>xy$x])
#   tp 		<- min(ages[ages>xy$y])
#	btm 	<- max(ages[ages<xy$y])
		lft 	<- floor(xy$x)
		rght 	<- ceiling(xy$x)
		tp 		<- ceiling(xy$y)
		btm 	<- floor(xy$y)
	
	    if ((xy$x - lft) > (xy$y - btm)){
			polygon(c(lft, rght, rght), c(btm, btm, tp), col = col, ...)
		} else {
			polygon(c(lft, rght, rght), c(btm, btm, tp), col = col, ...)	
		}
	}
}

