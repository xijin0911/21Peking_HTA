f_gen_psa <- function(n_sim = 1000, c_Trt = 50){
  
  require(truncnorm)
  
  df_psa <- data.frame(
    
    # Transition probabilities (per cycle)
    
    # prob Healthy -> Sick
    p_HS1   = rbeta(n = n_sim,
                    shape1 =  30,
                    shape2 =  170),
    
    # prob Sick    -> Healthy
    p_S1H   = rbeta(n = n_sim,
                    shape1 =  60,
                    shape2 =  60) ,
    
    # prob Sick    -> Sicker
    p_S1S2  = rbeta(n = n_sim,
                    shape1 =  84,
                    shape2 =  716),       
    
    # prob Healthy -> Dead
    p_HD    = rbeta(n = n_sim,
                    shape1 =  10,
                    shape2 =  1990),
    
    # rate ratio death S1 vs healthy
    hr_S1   = rlnorm(n = n_sim,
                     meanlog =  log(3),
                     sdlog =   0.01),
    
    # rate ratio death S2 vs healthy
    hr_S2   = rlnorm(n = n_sim,
                     meanlog =  log(10),
                     sdlog =  0.02),   
    
    # Cost vectors with length n_sim
    # cost p/cycle in state H
    c_H   = rgamma(n = n_sim, 
                   shape = 100, 
                   scale = 20),
    
    # cost p/cycle in state S1
    c_S1  = rgamma(n = n_sim, 
                   shape = 177.8, 
                   scale = 22.5),
    
    # cost p/cycle in state S2
    c_S2  = rgamma(n = n_sim,
                   shape = 225,
                   scale = 66.7),
    
    # cost p/cycle in state D
    c_D   = 0,
    
    # cost p/cycle of treatment
    c_Trt = c_Trt,
    
    # Utility vectors with length n_sim
    # utility when healthy
    u_H   = rtruncnorm(n = n_sim, 
                       mean = 1, 
                       sd = 0.01, 
                       b = 1),
    
    # utility when sick
    u_S1  = rtruncnorm(n = n_sim, 
                       mean = 0.75, 
                       sd = 0.02, 
                       b = 1),
    
    # utility when sicker
    u_S2  = rtruncnorm(n = n_sim, 
                       mean = 0.50,
                       sd = 0.03, 
                       b = 1),
    # utility when dead
    u_D   = 0,
    
    # utility when being treated
    u_Trt = rtruncnorm(n = n_sim,
                       mean = 0.95,
                       sd = 0.02, 
                       b = 1)  
  )
  
  return(df_psa)
}

f_MM_sicksicker <- function(params) {
  # run following code with a set of data 
  with(as.list(params), {
    
    # rate of death in healthy
    r_HD    = - log(1 - p_HD) 
    # rate of death in sick
    r_S1D   = hr_S1 * r_HD 	  
    # rate of death in sicker
    r_S2D   = hr_S2 * r_HD  
    # probability of death in sick
    p_S1D   = 1 - exp(-r_S1D) 
    # probability of death in sicker
    p_S2D   = 1 - exp(-r_S2D) 
    # calculate discount weight for each cycle
    v_dwe <- v_dwc <- 1 / (1 + d_r) ^ (0:n_t) 
    #transition probability matrix for NO treatment
    m_P <- matrix(0,
                  nrow = n_states, 
                  ncol = n_states,
                  dimnames = list(v_n, v_n))
    # fill in the transition probability array
    
    ### From Healthy
    m_P["H", "H"]  <- 1 - (p_HS1 + p_HD)
    m_P["H", "S1"] <- p_HS1
    m_P["H", "D"]  <- p_HD
    ### From Sick
    m_P["S1", "H"]  <- p_S1H
    m_P["S1", "S1"] <- 1 - (p_S1H + p_S1S2 + p_S1D)
    m_P["S1", "S2"] <- p_S1S2
    m_P["S1", "D"]  <- p_S1D
    ### From Sicker
    m_P["S2", "S2"] <- 1 - p_S2D
    m_P["S2", "D"]  <- p_S2D
    ### From Dead
    m_P["D", "D"] <- 1
    # create empty Markov trace 
    m_TR <- matrix(data = NA, 
                   nrow = n_t + 1, 
                   ncol = n_states, 
                   dimnames = list(0:n_t, v_n))     
    
    # initialize Markov trace
    m_TR[1, ] <- c(1, 0, 0, 0)          
    
    ############ PROCESS #####################
    
    for (t in 1:n_t){ # throughout the number of cycles
      # estimate next cycle (t+1) of Markov trace
      m_TR[t + 1, ] <- m_TR[t, ] %*% m_P           
    }
    
    ########### OUTPUT  ######################
    
    # create vectors of utility and costs for each state
    v_u_trt    <- c(u_H, u_Trt, u_S2, u_D)
    v_u_no_trt <- c(u_H, u_S1, u_S2, u_D)
    v_c_trt    <- c(c_H, c_S1 + c_Trt,
                    c_S2 + c_Trt, c_D)
    
    v_c_no_trt <- c(c_H, c_S1, c_S2, c_D)
    # estimate mean QALYs and costs
    v_E_no_trt <- m_TR %*% v_u_no_trt
    
    v_E_trt    <- m_TR %*% v_u_trt
    
    v_C_no_trt <- m_TR %*% v_c_no_trt
    
    v_C_trt    <- m_TR %*% v_c_trt
    
    ### discount costs and QALYs
    #   1x31 %*% 31x1 -> 1x1
    
    te_no_trt <- t(v_E_no_trt) %*% v_dwe  
    te_trt    <- t(v_E_trt) %*% v_dwe
    
    tc_no_trt <- t(v_C_no_trt) %*% v_dwc
    
    tc_trt    <- t(v_C_trt)    %*% v_dwc
    
    results <- c(
      "Cost_NoTrt" = tc_no_trt, 
      "Cost_Trt"   = tc_trt, 
      "QALY_NoTrt" = te_no_trt, 
      "QALY_Trt" = te_trt,
      "ICER" = (tc_trt - tc_no_trt)/
        (te_trt - te_no_trt)
    )
    
    return(results)
  }) # end with function  
} # end f_MM_sicksicker function




f_wrapper <- function(
  
  #-- User adjustable inputs --#
  
  # age at baseline
  n_age_init = 25, 
  # maximum age of follow up
  n_age_max  = 110,
  # discount rate for costs and QALYS 
  d_r = 0.035,
  # number of simulations
  n_sim   = 1000,
  # cost of treatment
  c_Trt   = 50       
  
){
  
  # need to specify environment of inner functions (to use outer function enviroment)
  # alternatively - define functions within the wrapper function.
  environment(f_gen_psa)         <- environment()
  environment(f_MM_sicksicker)   <- environment()
  
  #-- Unadjustable inputs --#
  
  #  number of cycles
  n_t <- n_age_max - n_age_init
  # the 4 health states of the model:
  v_n <- c("H", "S1", "S2", "D") 
  # number of health states 
  n_states <- length(v_n) 
  
  #-- Create PSA Inputs --#
  
  df_psa <- f_gen_psa(n_sim = n_sim, 
                      c_Trt =  c_Trt)
  
  #--  Run PSA  --#
  
  # Initialize matrix of results outcomes
  m_out <- matrix(NaN, 
                  nrow = n_sim, 
                  ncol = 5,
                  dimnames = list(1:n_sim,
                                  c("Cost_NoTrt", "Cost_Trt",
                                    "QALY_NoTrt", "QALY_Trt",
                                    "ICER")))
  
  # run model for each row of PSA inputs
  for(i in 1:n_sim){
    
    # store results in row of results matrix
    m_out[i,] <- f_MM_sicksicker(df_psa[i, ])
    
  } # close model loop
  
  
  #-- Return results --#
  
  # convert matrix to dataframe (for plots)
  df_out <- as.data.frame(m_out) 
  
  # output the dataframe from the function  
  return(df_out) 
  
} # end of function