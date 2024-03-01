qapply <- function(x){
  tapply(x,x,length)
}

fullyeardiff <- function(fromdate,todate){
  diff <- year(todate)-year(fromdate)
  diff <- diff-ifelse(month(fromdate)>month(todate),1,0)
  diff <- diff-ifelse((month(fromdate)==month(todate)) & (mday(fromdate)>mday(todate)),1,0)
  return(diff)
}