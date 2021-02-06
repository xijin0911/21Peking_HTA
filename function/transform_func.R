
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

plot_trace <- function(m_M) {
  df_M      <- data.frame(Cycle = 0:n_t, m_M, check.names = F)
  df_M_long <- tidyr::gather(df_M, key = `Health State`, value, 2:ncol(df_M))
  df_M_long$`Health State` <- factor(df_M_long$`Health State`, levels = v_names_states)
  p <- ggplot2::ggplot(df_M_long, aes(x = Cycle, y = value, 
                                      color = `Health State`, linetype = `Health State`)) +
    geom_line(size = 1) +
    xlab("Cycle") +
    ylab("Proportion of the cohort") +
    theme_bw(base_size = 14) +
    theme(legend.position  = "bottom", 
          legend.background = element_rect(fill = NA)) 
  
  return(p) 
}
