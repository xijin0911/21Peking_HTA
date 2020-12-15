
RateToProb <- function(rate, t){ # t is the cycle length
  prob <- 1-exp(-rate*t)
  prob
}


ProbToOdds <- function(prob){
  odds <- prob/(1-prob)
  odds
}

OddsToProb <- function(odds){
  prob <- odds/(1+odds)
  prob
}

# First converts the probability to odds
# then multiplies it by the given factor
# then converts it back to a probability
ProbFactor <- function(prob,factor){
  OddsToProb(factor*ProbToOdds(prob))  
}