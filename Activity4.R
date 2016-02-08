v <- c(1,0,0,0,0,0,0,0,0)
rm(list = ls())
violations <- function(x, testType = "m"){
  newVec <- x - log10(1+1/1:9)
  m <- (max(newVec))
  d <- sqrt(sum(newVec^2))
  am <- NULL
  if(m > .851) {am <- .1}
  if(m > .967) {am <-.05}
  if(m > 1.212) {am <- .01}
  dm <- NULL
  if(d > 1.212) {dm <- .1}
  if(d > 1.330) {dm <-.05}
  if(d > 1.569) {dm <- .01}
  ifelse(testType == "m", return(c(m,am)), return(c(d,dm)))
}





