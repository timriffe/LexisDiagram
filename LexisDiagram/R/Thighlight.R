Thighlight <-
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

