v <- c(.3,.5,.1,.1,0,0,0,.2,.2)
# 1
# @Param votes is the vector or matrix of vote totals
# @Param test_M is whether or not one wants the M statistic
# @Param test_D is whether or not one wants the D statistic
violations <- function(votes, test_M = TRUE, test_D = TRUE){
  vote_vector <- as.vector(votes)
  #Compute the similar part of the two statistics
  similar_comp <- vote_vector - log10(1+1/1:9)
  #Computes m if m is wanted
  ifelse(test_M, m <- (max(similar_comp)), m <- NULL)
  #Computes d if d is wanted
  ifelse(test_D, d <- sqrt(sum(similar_comp^2)), d <- NULL)
  #Figures out the percentage of the total of each vote count
  proportion <- paste((vote_vector/sum(vote_vector))*100, "%", sep = "")
  return(list("M" = m, "D" = d,"Proportion" = proportion, "Election_returns" = vote_vector))
}
violations(v)

#2

am <- NULL
if (m > .851) {am <- .1}
if (m > .967) {am <-.05}
if (m > 1.212) {am <- .01}
dm <- NULL
if (d > 1.212) {dm <- .1}
if (d > 1.330) {dm <-.05}
if (d > 1.569) {dm <- .01}
?return





