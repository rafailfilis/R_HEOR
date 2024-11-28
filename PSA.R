
library(ggplot2)

set.seed(123)
params <- list(
 
    # costs
    c_response = 500,
    c_remission = 300,
    c_no_response =  1500  , 
    c_dead = 0,

    #  QALYS

    q_remission = 0.92,
    q_response =  0.8,
    q_no_response = 0.5 ,
    q_dead = 0 
) 
# # what if a PSA is conducted
# # random values for costs, following a gamma distribution and random values for effects either following
# # a beta distribution (0 to 1 like QALYs) and a normal distribution.

n_psa <- 1000

params_PSA <- data.frame(
    # costs
    c_response = rgamma(n_psa, shape = 5, scale = params$c_response /5),
    c_remission = rgamma(n_psa, shape = 5, scale = params$c_remission /5),
    c_no_response = rgamma(n_psa, shape = 5, scale = params$c_no_response /5),
    c_dead = rep(0, n_psa),


    #  QALYS

    q_response = rnorm(n_psa, params$q_response, 0.1),
    q_remission = rnorm(n_psa, params$q_remission, 0.1),
    q_no_response = rnorm(n_psa, params$q_no_response, 0.15),
    q_dead = rep(0, n_psa)
)

states <- c("response", "remission" ,  "no-response", "dead" )
transition_probs <- c(
      0.75 , 0.10 , 0.13 , 0.02,
      0    , 0.64 , 0.26 , 0.10 , 
      0    , 0    , 0.75 ,  0.25,
      0, 0 ,0 ,1 
)




 

# matrix for fixed probabilities
t_matrix <- matrix(transition_probs, ncol = length(states) , nrow = length(states) , byrow = T, dimnames = list(from = states, to = states) )
 
 
 markov_table <- data.frame(
      response = NA,
      remission = NA ,
      no_response = NA,
      dead = NA
 )
 
 df_payoffs <-data.frame(
    ID = 1:n_psa,
      Cost = numeric(n_psa) ,
      QALYs =  numeric(n_psa)
       
 ) 
    print(t_matrix)
n_cycle <- 40


# create the markov simulation model frame
markov_table[1, ] <- c(1000, 0 , 0 , 0)
       



run_psa <- function(n_cohort, n_cycle, markov_table, t_matrix ){
     psa_data <- split(params_PSA, 1:n_psa)
         for(j in 2:n_cycle){
              markov_table[j,] <- as.matrix(markov_table[j-1, ]) %*%  t_matrix 
           }

      for(i in  seq_along(psa_data)){
         
        for(j in length(n_cycle)){

     
        #    payoffs
        m_payoffs <- matrix(0, ncol = 2, nrow = length(states) , dimnames = list(states , c("Cost", "QALYs")))
        
        m_payoffs[1, "Cost"] <- psa_data[[i]]$c_response
        m_payoffs[2, "Cost"] <- psa_data[[i]]$c_remission
        m_payoffs[3, "Cost"] <- psa_data[[i]]$c_no_response
        m_payoffs[4, "Cost"] <- psa_data[[i]]$c_dead

        m_payoffs[1, "QALYs"] <- psa_data[[i]]$q_response * 8 # cycle length to get the QALYs e.x 8 weeks / cycle
         m_payoffs[2, "QALYs"] <- psa_data[[i]]$q_remission * 8
          m_payoffs[3, "QALYs"] <- psa_data[[i]]$q_no_response * 8
           m_payoffs[4, "QALYs"] <- psa_data[[i]]$q_dead * 8

           
  
           payoff_trace <-   as.matrix(markov_table[j, ]) %*% m_payoffs 
        #    the payoff trace calculates the costs and qalys for each cycle 
    
           total_cost <- sum(payoff_trace[, "Cost"])
           total_qalys <- sum(payoff_trace[, "QALYs"])

          
            #    push the payofff for each psa sim into the data frame divided by the n_cohort
            #  to find the cost and qalys per patient 
           
           df_payoffs[i, "Cost"] <- total_cost / n_cohort
           df_payoffs[i,  "QALYs"] <- total_qalys / n_cohort
               
        } 
        
            
      } 
 
       return(df_payoffs)
    
    
}
 
df_payoffs <- run_psa(1000, 40, markov_table, t_matrix )   
 plot <- ggplot(df_payoffs, aes(x = Cost)) + geom_histogram(aes(y = ..density..), bins = 30, fill = "green" ,colour= "blue")  + geom_density(color = "red", size = 1.2) + theme_light()

 print(plot)