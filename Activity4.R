v <- c(3,51,20,10,0,0,0,543,226)
# 1
# Args:
#   votes: A vector or matrix of vote returns
#   test_M: If True does the M statistic, if False it doesnt
#   test_D: If True does the D statistic, if False it doesnt
# Returns:
#   The M and D statistics if asked for, the percentage that each digit occurs, 
#   and a vector of vote returns

benfords.law <- function(votes, test_M = TRUE, test_D = TRUE){
  vote_vector <- as.vector(votes)
  # Creates a vector of the first digits
  digit_one <- as.numeric(substr(vote_vector,start=1, stop=1))
  # Count the number of each first digit
  digit_vector <- NULL
  for (i in 1:9) {
    digit_vector <- c(digit_vector, length(digit_one[digit_one == i]))
  }
  # Compute the similar part of the two statistics
  similar_comp <- digit_vector - log10(1+1/1:9)
  # Computes m if m is wanted
  ifelse(test_M, m <- (max(similar_comp)), m <- NULL)
  # Computes d if d is wanted
  ifelse(test_D, d <- sqrt(sum(similar_comp^2)), d <- NULL)
  # Figures out the percentage of the total of each vote count
  p <- round((digit_vector/sum(digit_vector))*100, 2)
  proportion <- paste(p, "%", sep = "")
  return(list("M" = m, "D" = d,"Proportion" = proportion, "Election_returns" = vote_vector))
}
benfords.law(v)

#2
print.benfords <- function(votes){
  # analyzes the data using the function to do the statistics
  ben_law <- benfords.law(votes)
  m <- ben_law$M
  d <- ben_law$D
  # ma is the critical value for m statistic
  ma <- NULL
  if (m > .851) {ma <- "*"}
  if (m > .967) {ma <- "**"}
  if (m > 1.212) {ma <- "***"}
  # da is the critical value for the d statistic
  da <- NULL
  if (d > 1.212) {da <- "*"}
  if (d > 1.330) {da <- "**"}
  if (d > 1.569) {da <- "***"}
  
  
  return()
}




